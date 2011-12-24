{-# OPTIONS_GHC -fno-warn-unused-binds #-} --TODO

module Analyzer (
    printRecommendationFromAnalysis,
    printAdviceFromAnalysis,
    analyze,
    ) where

import Defs.Structures
import Defs.Util
import Defs.Common
import Defs.AST

import Recommend
import Advice

import Data.List
import Data.Function
import Data.Monoid
import Data.Maybe
import Data.Maybe.HT
import Control.Monad.State
import Control.Arrow
import Safe

-- | Data structure for analysis info
data DSInfo = DSI {
    getDSINames  :: [(FunctionName, VariableName)],     -- ^ Variable holding the data structure --FIXME pointer copying
    getDSIDSU      :: [DSUse]                           -- ^ Data structure use cases
    } deriving (Show, Eq)

instance Monoid DSInfo where
    mempty = DSI [] []
    mappend (DSI n1 d1) (DSI n2 d2) = DSI (n1 `union` n2) (d1 `union` d2)

-- | Data structure for function info
data DSFun = DSF {
    getDSFFun   :: Function,
    getDSFCalls :: [(FunctionName, [Maybe VariableName])],
    getDSFDSI   :: [DSInfo]
    } deriving (Show, Eq)

-- | Data structure for use case info
data DSUse = DSU {
    getDSUName      :: OperationName,   -- ^ Operation used
    isHeavilyUsed   :: Bool,            -- ^ Is it heavily used
    isUserDependent :: Bool             -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
    } deriving (Show, Eq)

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

type TermAnalyzerOutput = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateFunction :: Function,                               -- ^ Current function being analyzed
    getStateCalls    :: [(FunctionName, [Maybe VariableName])]  -- ^ Function calls gathered through the analysis
    } deriving (Show, Eq)

append :: TermAnalyzerState -> TermAnalyzerState -> TermAnalyzerState
append (AS f1 cs1) (AS _ cs2) = AS f1 (cs1 `union` cs2)

-- | Pretty print single 'DSInfo'
printDSI :: (String -> IO ()) -> DSInfo -> IO ()
printDSI output dsi = do
    output "The recommended structure for "
    redColor
    output $ show (getDSINames dsi) ++ "\n"
    resetColor
    output " is:"
    cyanColor
    recommendedDS >>= output.show
    resetColor where
        recommendedDS = do
            let opns = map getDSUName $ getDSIDSU dsi
            recommendDS opns

printDSIAdvice :: (String -> IO ()) -> DSInfo -> IO ()
printDSIAdvice output dsi = do
    let opns = map getDSUName $ getDSIDSU dsi
    printAdvice output opns

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO()
printRecommendationFromAnalysis output = mapM_ (printDSI output)

printAdviceFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO ()
printAdviceFromAnalysis output = mapM_ (printDSIAdvice output)

-- | Stupid merging of dsis --TODO remove this function, rewrite analyzeFunctions correctly
stupidMerge ::  [DSInfo] -> [DSInfo]
stupidMerge (dsi:dsis) = let (same, different) = partition (\dsi' -> getDSINames dsi `intersect` getDSINames dsi' /= []) dsis in
    mconcat (dsi:same) : stupidMerge different
stupidMerge [] = []

-- | Runs everything that is needed to analyze a program
analyze :: [Function] -> [DSInfo]
analyze functions = let dsfs = map generateDSF functions in
    stupidMerge $ analyzeFunctions dsfs

-- | Merges the simple 'DSInfo's based on function calls from the functions
analyzeFunctions :: [DSFun] -> [DSInfo]
analyzeFunctions dsfs = let startingDSF = lookupDSF dsfs startingFunction in
    let functions = map getDSFFun dsfs in
    let startingVars = map snd $ concatMap getDSINames $ getDSFDSI startingDSF in
    let runMain = mapMaybe (\var -> analyzeFunction functions startingDSF var []) startingVars in --update the accumulator
    concatMap (uncurry (:)) runMain where

        analyzeFunction :: [Function] -> DSFun -> VariableName -> [FunctionName] -> Maybe (DSInfo, [DSInfo])
        analyzeFunction functions dsf variable accumulator = let functionName = getFunName.getDSFFun $ dsf in
            toMaybe (functionName `notElem` accumulator) (let functionCalls = getDSFCalls dsf in
                    let relevantFunctionCalls = filter (\(_, funArgs) -> Just variable `elem` funArgs) functionCalls in
                    let irrelevantFunctionCalls = functionCalls \\ relevantFunctionCalls in --TODO remodel so we also analyze those
                    let dsis = getDSFDSI dsf in
                    let thisVariableDSI = lookupDSI dsis variable functionName in
                    let otherVariablesDSIs = dsis \\ [thisVariableDSI] in
                    let variableBindings = map (\call@(funName, _) -> second (bindFuncall functions funName) call) relevantFunctionCalls in
                    let recursiveCalls = mapMaybe (\(funName, varPairs) -> (analyzeFunction functions (lookupDSF dsfs funName) (lookupJust variable varPairs) (funName:accumulator))) variableBindings in
                    let relevantRecursiveDSI = mconcat $ map fst recursiveCalls in
                    let irrelevantRecursiveDSI = concatMap snd recursiveCalls in
                    (thisVariableDSI `mappend` relevantRecursiveDSI, otherVariablesDSIs `union` irrelevantRecursiveDSI))

-- | Lookup 'DSFun' by 'FunctionName'
lookupDSF :: [DSFun] -> FunctionName -> DSFun
lookupDSF dsfs functionName = lookupJust functionName (zip (map (getFunName.getDSFFun) dsfs) dsfs)

-- | Lookup 'DSInfo' by 'FunctionName' and 'VariableName'
lookupDSI :: [DSInfo] -> VariableName -> FunctionName -> DSInfo
lookupDSI dsis variable functionName = findJust (\dsi -> (functionName, variable) `elem` getDSINames dsi) dsis

-- | Returns pairs of local variables bound to variables in a function that is called
bindFuncall :: [Function] -> FunctionName -> [Maybe VariableName] -> [(VariableName, VariableName)]
bindFuncall functions functionName vns = let
    function = findJust (\function -> getFunName function == functionName) functions in
    maybeZipWith bindZipper vns (map fst (getFunArgs function)) where
        bindZipper :: Maybe VariableName -> VariableName -> Maybe (VariableName, VariableName)
        bindZipper (Just a) b = Just (a,b)
        bindZipper Nothing _ = Nothing

-- | Generates simple 'DSInfo's without the info from function calls
generateDSI :: Function -> [(VariableName, DSUse)] -> [DSInfo]
generateDSI fn dsus = let varGroups = groupBy (on (==) fst) dsus in
    map (\g -> DSI [(getFunName fn, fst.head $ g)] (map snd g)) varGroups

-- | Start the state monad to create a 'DSFun' for function
generateDSF :: Function -> DSFun
generateDSF fn = let (dsus, st) = runState (sumTerms step [getFunBody fn]) (AS fn []) in
    DSF fn (getStateCalls st) (generateDSI fn dsus)

-- | Analyze a block of terms using the state monad
stepBlock :: [Term] -> TermAnalyzer TermAnalyzerOutput
stepBlock = sumTerms step

-- | Sums the 'DSUse's using the 'step' function and concating the results
sumTerms :: (Term -> TermAnalyzer TermAnalyzerOutput) -> [Term] -> TermAnalyzer TermAnalyzerOutput
sumTerms f ts = fmap concat (mapM f ts)

-- | Function putting a function call in the state
putCall :: FunctionName -> [Term] -> TermAnalyzer ()
putCall name args = do
    let cleanArgs = map justifyVars args
    let call = (name, cleanArgs)
    modify  $ \s -> s {getStateCalls = call:getStateCalls s} where
        justifyVars :: Term -> Maybe VariableName
        justifyVars (Var v) = Just v -- FIXME should work on function calls returning dses, not only vars
        justifyVars _ = Nothing


-- | Generate 'DSUse's for a single 'Term'
step :: Term -> TermAnalyzer TermAnalyzerOutput

step (Block body) = stepBlock body

step (VarInit _ Ds) = return []
step (InitAssign _ _ Ds) = return []

step (While cond body) = stepBlock [cond,body]

step (Funcall name args) = do
    let opname = case name of -- FIXME nicer with usage of dsinfFunctions from Common
            F "insert"        -> Just InsertVal
            F "find"          -> Just FindByVal
            F "update"        -> Just UpdateByRef
            F "max"           -> Just ExtremalVal
            F "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing
                                            -- FIXME add reading the function calls
    argDsus <- stepBlock args

    funcallDsu <- case opname of
        Nothing ->  do
            putCall name args
            return []
        Just op ->  case head args of       -- FIXME dsinfFunctions ds argument recognition
            Var varname -> return [(varname, DSU op False False)]
            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu

step (If cond t1 t2) = do
    dsuCond <- step cond
    oldState <- get

    dsuT1 <- step t1
    stateT1 <- get

    put oldState

    dsuT2 <- step t2
    stateT2 <- get

    put (stateT1 `append` stateT2)
    return $ concat [dsuCond, dsuT1, dsuT2]

-- Dummy steps
step t = case t of
    Var _ -> return []
    VarInit _ _ -> return []
    Inc _ -> return []
    Dec _ -> return []
    Int _ -> return []

    Assign _ t -> step t
    Lt t1 t2 -> stepBlock [t1, t2]
    Mul t1 t2 -> stepBlock [t1, t2]
    s -> error $ "No step for " ++ show s
