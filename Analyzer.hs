module Analyzer 
  ( printRecommendationFromAnalysis,
    analyze) where

import Defs.Structures
import Defs.Util
import Defs.Common
import Defs.AST

import Typechecker
import Recommend

import Data.List
import Control.Monad.State

-- | Data structure for analysis info
data DSInfo = DSI { 
    getDSIName  :: [(FunctionName, VariableName)],  -- ^ Variable holding the data structure --FIXME pointer copying
    getDSU      :: [DSUse]                          -- ^ Data structure use cases
    } deriving (Show, Eq)

-- | Data structure for function info
data DSFun = DSF { 
    getDSFDef   :: Function,      
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


setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True

-- | Pretty print single 'DSInfo' 
printDSI :: DSInfo -> IO()
printDSI dsi = do
    putStr "The recommended structure for "
    redColor
    print $ getDSIName dsi
    resetColor 
    putStrLn " is:"
    cyanColor
    recommendedDS >>= print
    resetColor where
        recommendedDS = do 
            let opns = map getDSUName $ getDSU dsi
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
closeDSIs = undefined



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
foldlTerms f start [] = return start
foldlTerms f start (r:rest) = do 
    dsus <- f start r
    foldlTerms f dsus rest

-- | Function putting a variable definition in the context
putVar :: VariableName -> Analyzer ()
putVar name = do
    state <- get 
    put $ AS (getStateFunction state) (getStateFunNames state) (name:getStateVarNames state) (getStateCalls state)

-- | Function returning 'True' if the variable is already defined
getVar :: VariableName -> AnalyzerState -> Bool
getVar name state = name `elem` getStateVarNames state

-- | Function putting a function call in the state
putCall :: FunctionName -> [Term] -> Analyzer () 
putCall name args = do
    state <- get
    let cleanArgs = map justifyVars args
    let call = (name, cleanArgs) 
    put $ AS (getStateFunction state) (getStateFunNames state) (getStateVarNames state) (call:getStateCalls state) where
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

step dsus (VarInit name _) = return dsus

step dsus (InitAssign name term Ds) = do
    s <- get
    if getVar name s
        then error $ name ++ " already initialized"
        else putVar name >> return dsus 

step dsus (InitAssign name term _ ) = return dsus

step dsus (While cond body) = do
    newDSU <- stepBlock [cond,body] 
    return $ dsus ++ map (\(b,c) -> (b,setHeavyUsage c)) newDSU -- FIXME smarter heavy load recognition

step dsus (Funcall name args) = do 
    s <- get 
    let opname = case name of                   -- FIXME nicer with usage of dsinfFunctions from Common
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

--step dsus _ = return dsus
