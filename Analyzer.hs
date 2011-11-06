module Analyzer (
    printRecommendationFromAnalysis,
    analyze
    ) where

import Defs.Structures
import Defs.Util
import Defs.Common
import Defs.AST

import Recommend

import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Arrow

-- | Data structure for analysis info
data DSInfo = DSI {
    getDSINames  :: [(FunctionName, VariableName)],  -- ^ Variable holding the data structure --FIXME pointer copying
    getDSIDSU      :: [DSUse]                          -- ^ Data structure use cases
    } deriving (Show, Eq)

instance Monoid DSInfo where
    mempty = DSI [] []
    mappend (DSI n1 d1) (DSI n2 d2) = DSI (n1++n2) (nub $ d1++d2)

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

-- | State monad with 'AnalyzerState'
type Analyzer a = State AnalyzerState a

type AnalyzerOutput = [(VariableName, DSUse)]

-- | State of the analyzer
data AnalyzerState = AS {
    getStateFunction :: Function,                               -- ^ Current function being analyzed
    getStateFunNames :: [FunctionName],                         -- ^ All the other function names
    getStateVarNames :: [VariableName],                         -- ^ All the variable names
    getStateCalls    :: [(FunctionName, [Maybe VariableName])]  -- ^ Function calls gathered through the analysis
    } deriving (Show, Eq)

setHeavyUsage ::  DSUse -> DSUse
setHeavyUsage (DSU opname _ ud) = DSU opname True ud
{- TODO lenses?
setUserDependance ::  DSUse -> DSUse
setUserDependance (DSU opname hu _) = DSU opname hu True
-}

-- | Pretty print single 'DSInfo'
printDSI :: DSInfo -> IO()
printDSI dsi = do
    putStr "The recommended structure for "
    redColor
    print $ getDSINames dsi
    resetColor
    putStrLn " is:"
    cyanColor
    recommendedDS >>= print
    resetColor where
        recommendedDS = do
            let opns = map getDSUName $ getDSIDSU dsi
            recommendDS opns

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: [DSInfo] -> IO()
printRecommendationFromAnalysis = mapM_ printDSI

-- | Runs everything that is needed to analyze a program
analyze :: [Function] -> [DSInfo]
analyze functions = let functionNames = map getFunName functions in
    let dsfs = map (generateDSF functionNames) functions in
    closeDSIs dsfs

-- | Merges the simple 'DSInfo's based on function calls from the functions
closeDSIs :: [DSFun] -> [DSInfo]
closeDSIs dsfs = let startingDSF = lookupFun startingFunction in
    let dsiVars = map snd $ filter (\(fn,_) -> fn == startingFunction) $ concatMap getDSINames $ getDSFDSI startingDSF in
    concatMap (\var -> closeDSIs' startingDSF var []) dsiVars where

        closeDSIs' :: DSFun -> VariableName -> [FunctionName] -> [DSInfo]
        closeDSIs' dsf var accu = let funname = getFunName (getDSFFun dsf) in
            if funname `elem` accu
                then []
                else let funcalls = getDSFCalls dsf in
                    let varConts = concatMap bindFuncall funcalls in
                    let dsis = getDSFDSI dsf in
                    let currDSI = lookupDSI dsis funname var in
                    let otherDSI = dsis \\ [currDSI] in
                    mconcat (currDSI:concatMap (\(fn, vn) -> (closeDSIs' (lookupFun fn) vn (funname:accu))) varConts):otherDSI where

                        bindFuncall :: (FunctionName, [Maybe VariableName]) -> [(VariableName, VariableName)]
                        bindFuncall  = bindFuncall' 1

                        bindFuncall' :: Int -> (FunctionName, [Maybe VariableName]) -> [(FunctionName, VariableName)]
                        bindFuncall' n (fn, Just vn:vns) = if vn == var
                            then (fn, getNewVarName fn n): bindFuncall' (n+1) (fn, vns)
                            else bindFuncall' (n+1) (fn, vns)
                        bindFuncall' n (fn, Nothing:vns) = bindFuncall' (n+1) (fn, vns)
                        bindFuncall' _ (_, []) = []



                        lookupDSI :: [DSInfo] -> FunctionName -> VariableName -> DSInfo
                        lookupDSI dsis funname var1 = let goodDSI = filter (\dsi -> (funname, var1) `elem` getDSINames dsi) dsis in
                            if length goodDSI /= 1
                                then error $ "None or too many matching DSI " ++ show (funname, var1)
                                else head goodDSI


        lookupFun :: FunctionName -> DSFun
        lookupFun name = let goodDsfs = filter (\dsf -> getFunName (getDSFFun dsf) == name) dsfs in
            if length goodDsfs /= 1
                then error $ "None or too many matching functions " ++ name
                else head goodDsfs

        getNewVarName :: FunctionName -> Int -> VariableName
        getNewVarName fn n = let fun = getDSFFun $ lookupFun fn in
           fst $ getFunArgs fun !! n

-- | Generates simple 'DSInfo's without the info from function calls
generateDSI :: Function -> [(VariableName, DSUse)] -> [DSInfo]
generateDSI fn dsus = let varGroups = groupBy (\(varname1,_) (varname2,_) -> varname1 == varname2) dsus in
    map (\g -> DSI [(getFunName fn, fst.head $ g)] (map snd g)) varGroups

-- | Start the state monad to create a 'DSFun' for function
generateDSF :: [FunctionName] -> Function -> DSFun
generateDSF fnns fn = let (dsus, st) = runState (foldlTerms step [] [getFunBody fn]) (AS fn fnns [] []) in
    DSF fn (getStateCalls st) (generateDSI fn dsus)

-- | Analyze a block of terms using the state monad
stepBlock :: [Term] -> Analyzer AnalyzerOutput
stepBlock = foldlTerms step [] where

-- | Foldl 'Term's using the 'step' function to generate 'AnalyzerOutput'
foldlTerms :: (AnalyzerOutput -> Term -> Analyzer AnalyzerOutput) -> AnalyzerOutput -> [Term] -> Analyzer AnalyzerOutput
foldlTerms _ start [] = return start
foldlTerms f start (r:rest) = do
    dsus <- f start r
    foldlTerms f dsus rest

-- | Function putting a variable definition in the context
putVar :: VariableName -> Analyzer ()
putVar name = do
    s <- get
    put $ AS (getStateFunction s) (getStateFunNames s) (name:getStateVarNames s) (getStateCalls s)

-- | Function returning 'True' if the variable is already defined
getVar :: VariableName -> AnalyzerState -> Bool
getVar name s = name `elem` getStateVarNames s

-- | Function putting a function call in the state
putCall :: FunctionName -> [Term] -> Analyzer ()
putCall name args = do
    s <- get
    let cleanArgs = map justifyVars args
    let call = (name, cleanArgs)
    put $ AS (getStateFunction s) (getStateFunNames s) (getStateVarNames s) (call:getStateCalls s) where
        justifyVars :: Term -> Maybe VariableName
        justifyVars (Var v) = Just v -- FIXME should work on function calls returning dses, not only vars
        justifyVars _ = Nothing

-- | Folding step generating 'DSUse's
step :: AnalyzerOutput -> Term -> Analyzer AnalyzerOutput


step dsus (Block body) = do
    newDSU <- stepBlock body
    return $ dsus ++ newDSU

step dsus (VarInit name Ds) = do
    s <- get
    if getVar name s
        then error $ name ++ " already initialized"
        else putVar name >> return dsus


step dsus (InitAssign name _ Ds) = do
    s <- get
    if getVar name s
        then error $ name ++ " already initialized"
        else putVar name >> return dsus

step dsus (InitAssign _ _ _) = return dsus

step dsus (While cond body) = do
    newDSU <- stepBlock [cond,body]
    return $ dsus ++ map (second setHeavyUsage) newDSU -- FIXME smarter heavy load recognition

step dsus (Funcall name args) = do
    s <- get
    let opname = case name of -- FIXME nicer with usage of dsinfFunctions from Common
            "insert"        -> Just InsertVal
            "find"          -> Just FindByVal
            "update"        -> Just UpdateByRef
            "max"           -> Just ExtremalVal
            "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing
                                            -- FIXME add reading the function calls
    argDsus <- stepBlock args

    funcallDsu <- case opname of
        Nothing ->  do
            putCall name args
            return []
        Just op ->  case head args of       -- FIXME dsinfFunctions ds argument recognition
            Var varname -> if getVar varname s
                then return [(varname, DSU op False False)]
                else error $ varname ++ " not initialized before use in function " ++ name
            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu ++ dsus

step dsus (If cond t1 t2) = do
    dsuCond <- stepBlock [cond]
    oldState <- get

    dsuT1 <- stepBlock [t1]
    stateT1 <- get

    put oldState

    dsuT2 <- stepBlock  [t2]
    stateT2 <- get

    put $ AS (getStateFunction stateT1) (getStateFunNames stateT1) (getStateVarNames stateT1 `union` getStateVarNames stateT2) (getStateCalls stateT1 `union` getStateCalls stateT2)
    return $ dsus ++ concat [dsuCond, dsuT1, dsuT2]


-- Dummy steps
step dsus t = case t of
    Var _ -> return dsus
    VarInit _ _ -> return dsus
    Inc _ -> return dsus
    Dec _ -> return dsus
    s -> error $ "No step for " ++ show s
