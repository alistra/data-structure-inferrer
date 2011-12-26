module Analyzer where
--TODO export list

import Data.Monoid
import Data.Maybe
import Data.Maybe.HT
import Data.List
import Data.Function
import Control.Monad.State
import Control.Arrow
import Safe

import Defs.Util
import Defs.Common
import Defs.Structures

import Advice
import Recommend

-- | Data structure use case
data DSUse = DSU {
    getDSUName      :: OperationName,                         -- ^ Operation used
    isHeavilyUsed   :: Bool,                                  -- ^ Is it heavily used
    isUserDependent :: Bool                                   -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
    } deriving (Show, Eq)

-- | Data structure for analysis info for one data-structure (possibly in many forms of different variables in functions)
data DSInfo = DSI {
    getDSINames :: [(FunctionName, VariableName)],            -- ^ Variables, used in functions, holding the analyzed data structure
    getDSIDSU   :: [DSUse]                                    -- ^ 'DSUse's of the data structure
    } deriving (Show, Eq)

instance Monoid DSInfo where
    mempty = DSI [] []
    mappend (DSI n1 d1) (DSI n2 d2) = DSI (n1 `union` n2) (d1 `union` d2)

-- | Data structure for analysis info of a function definition
data DSFun t = DSF {
    getDSFFun   :: FunctionDeclaration t,                     -- ^ Analyzed function declaration
    getDSFCalls :: [FunctionCall],                            -- ^ 'FunctionCall's from the analyzed function
    getDSFDSI   :: [DSInfo]                                   -- ^ 'DSInfo' about the variables inside of the function, at this stage are not yet ready to obtain the information
    } deriving (Show)

-- | Type for function definitions
data FunctionDeclaration t = FunDecl {
    getFunName :: FunctionName,                               -- ^ Name of the function
    getFunType :: t,                                          -- ^ Return type of the function
    getFunArgs :: [(VariableName, t)]                         -- ^ Names and types of the function arguments
    } deriving (Show)

-- | Function call - name of the function, relevant arguments
type FunctionCall = (FunctionName, [Maybe VariableName])

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

-- | Term analyzer basic output, variables and 'DSUse's
type Output = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateCalls    :: [FunctionCall]                        -- ^ 'FunctionCall's gathered through the analysis
    } deriving (Show)

instance Monoid TermAnalyzerState where
    mempty = AS []
    mappend (AS cs1) (AS cs2) = AS (cs1 `union` cs2)

-- | Name of the starting function
startingFunction :: FunctionName
startingFunction = F "main"

-- | Pretty print single 'DSInfo'
printDSI :: (String -> IO ()) -> DSInfo -> IO ()
printDSI output dsi = do
    output "The recommended structure for:\n"
    printDSINames $ getDSINames dsi
    output "is:\n"
    cyanColor
    recommendedDS >>= output . show
    output "\n"
    resetColor where
        recommendedDS = do
            let opns = map getDSUName $ getDSIDSU dsi
            recommendDS opns

        printDSINames [] = return ()
        printDSINames ((F fn,V vn):ns) = greenColor >> output vn >> resetColor >> output " in " >> greenColor >> output fn >> resetColor >> output "\n"

-- | Pretty print advice for a single 'DSInfo'
printDSIAdvice :: (String -> IO ()) -> DSInfo -> IO ()
printDSIAdvice output dsi = do
    let opns = map getDSUName $ getDSIDSU dsi
    printAdvice output opns

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO()
printRecommendationFromAnalysis output = mapM_ (printDSI output)

-- | Pretty printer for the advisor effects
printAdviceFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO ()
printAdviceFromAnalysis output = mapM_ (printDSIAdvice output)

-- | Stupid merging of dsis --TODO remove this function, rewrite analyzeFunctions correctly
stupidMerge ::  [DSInfo] -> [DSInfo]
stupidMerge (dsi:dsis) = let (same, different) = partition (\dsi' -> getDSINames dsi `intersect` getDSINames dsi' /= []) dsis in
    mconcat (dsi:same) : stupidMerge different
stupidMerge [] = []

-- | Merges the simple 'DSInfo's based on function calls from the functions
analyzeFunctions :: [DSFun t] -> [DSInfo]
analyzeFunctions dsfs = let startingDSF = lookupDSF dsfs startingFunction in
    let functions = map getDSFFun dsfs in
    let startingVars = map snd $ concatMap getDSINames $ getDSFDSI startingDSF in
    let runMain = mapMaybe (\var -> analyzeFunction functions startingDSF var []) startingVars in --update the accumulator
    concatMap (uncurry (:)) runMain where
        analyzeFunction :: [FunctionDeclaration t] -> DSFun t1 -> VariableName -> [FunctionName] -> Maybe (DSInfo, [DSInfo])
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
lookupDSF :: [DSFun t] -> FunctionName -> DSFun t
lookupDSF dsfs functionName = lookupJust functionName (zip (map (getFunName.getDSFFun) dsfs) dsfs)

-- | Lookup 'DSInfo' by 'FunctionName' and 'VariableName'
lookupDSI :: [DSInfo] -> VariableName -> FunctionName -> DSInfo
lookupDSI dsis variable functionName = findJust (\dsi -> (functionName, variable) `elem` getDSINames dsi) dsis

-- | Returns pairs of local variables bound to variables in a function that is called
bindFuncall :: [FunctionDeclaration t] -> FunctionName -> [Maybe VariableName] -> [(VariableName, VariableName)]
bindFuncall functions functionName vns = let
    function = findJust (\function -> getFunName function == functionName) functions in
    maybeZipWith bindZipper vns (map fst (getFunArgs function)) where
        bindZipper :: Maybe VariableName -> VariableName -> Maybe (VariableName, VariableName)
        bindZipper (Just a) b = Just (a,b)
        bindZipper Nothing _ = Nothing

-- | Generates simple 'DSInfo's without the info from function calls
generateDSI :: FunctionName -> Output -> [DSInfo]
generateDSI funName dsus = let varGroups = groupBy (on (==) fst) dsus in
    map (\g -> DSI [(funName, fst.head $ g)] (map snd g)) varGroups
