module Analyzer 
  ( printRecommendationFromAnalysis,
    analyze) where

import Defs.Structures
import Defs.Common
import Defs.AST

import Typechecker
import Recommend

import Data.List
import Control.Arrow
import Control.Monad.State

-- | Data structure analysis info type
data DSInfo = DSI { getDSIName :: Name,    -- ^ Variable holding the data structure
                    isStatic :: Bool,           -- ^ Is the variable static or dynamic
                    getDSU :: [DSUse]           -- ^ Data structure use cases
                    } deriving (Show, Eq)

data DSFun = DSF {  getDSFDef :: Function,
                    getDSFCalls :: [Function],
                    getDSFDSU :: [DSUse]
                    } deriving (Show, Eq)

-- | Data structure use case type
data DSUse = DSU {  getDSUName :: OperationName,  -- ^ Operation used
                    isHeavilyUsed :: Bool,          -- ^ Is it heavily used
                    isUserDependent :: Bool         -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
                    } deriving (Show, Eq)

-- | List of pairs: variable name, use case
type AnalyzerOutput = [(Name, DSUse)]

-- | State monad with 'AnalyzerState' 
type Analyzer a = State AnalyzerState a

-- | State containing function names defined in and current variables declared
data AnalyzerState = AS { getStateFunNames :: FunNames, getStateContext :: Context }

-- | Type for storing the function names defined in a program
type FunctionNames = [Name]

-- | Analyzer context containing variable names with data structures
type Context = [Name]

filter (\x -> getFunName x == f && (map fromJust tps) == (map snd (getFunArgs x))) (getStateFunctions s)

setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True

-- | Pretty print single 'DSInfo' 
printDSI :: DSInfo -> IO()
printDSI dsi = do
    putStr "The recommended structure for "
    redColor
    putStr $ getDSIName dsi
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
analyze fns = let   fnsNs = map getFunName fns
                    fnsDSU = zip fnsNs (map (generateDSU fnns) fns) in
                    generateDSI $ concatMap snd fnsDSU
      
-- | Generate 'DSInfo's for each data structure in the program
generateDSI :: AnalyzerOutput -> [DSInfo]
generateDSI allDSU@((name, _):dsus) = let (sameName, otherNames) = partition (\x -> fst x == name) allDSU in
    generateSingleDSI sameName : generateDSI otherNames
generateDSI [] = []

-- | Generate single 'DSInfo' from 'DSUse's of on data structure
generateSingleDSI :: AnalyzerOutput -> DSInfo
generateSingleDSI allNDSU@((name, _):_) = DSI name static cleanDSU where
    mergeDSU allDSU@(dsu:dsus) = let (sameOp, otherOps) = partition (\x -> getDSUName x == getDSUName dsu) allDSU in
        mergeSingleDSU sameOp : mergeDSU otherOps
    mergeDSU [] = []
    mergeSingleDSU = foldl1 (\(DSU name1 hu1 ud1) (DSU name2 hu2 ud2) -> DSU name1 (hu1 || hu2) (ud1 || ud2))
    cleanDSU = mergeDSU (map snd allNDSU)
    static = True

-- | Start the state monad to gather 'DSUse's from the AST
generateDSU :: [Name] -> Function -> AnalyzerOutput
generateDSU fnns fn = evalState (foldlTerms step [] [getFunBody fn]) (AS fnns [])

-- | Analyze the terms using the state monad
generateContextDSU :: [Term] -> Analyzer AnalyzerOutput
generateContextDSU = foldlTerms step [] where

-- | Foldl 'Term's using the 'step' function to generate 'AnalyzerOutput'
foldlTerms :: (AnalyzerOutput -> Term -> Analyzer AnalyzerOutput) -> AnalyzerOutput -> [Term] -> Analyzer AnalyzerOutput
foldlTerms f start [] = return start
foldlTerms f start (r:rest) = do 
    dsus <- f start r
    foldlTerms f dsus rest

-- | Function putting a variable definition in the context
putVar :: Name -> Analyzer ()
putVar name = do
    state <- get 
    put $ AS (getStateFunNames state) (name:getStateContext state)

-- | Function returning 'True' if the variable is already defined
getVar :: Name -> AnalyzerState -> Bool
getVar name state = name `elem` getStateContext state

-- | Folding step generating 'DSUse's
step :: AnalyzerOutput -> Term -> Analyzer AnalyzerOutput

step dsus (Block body) = do
    newDSU <- generateContextDSU body
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
    newDSU <- generateContextDSU [cond,body] 
    return $ dsus ++ map (Control.Arrow.second setHeavyUsage) newDSU -- FIXME

step dsus (Funcall name args) = do 
    s <- get 
    let opname = case name of
            "insert"        -> Just InsertVal
            "find"          -> Just FindByVal
            "update"        -> Just UpdateByRef -- FIXME
            "max"           -> Just ExtremalVal
            "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing 
                                                -- FIXME add reading the function calls
    argDsus <- generateContextDSU args

    funcallDsu <- case opname of
            Nothing ->  return []
            Just op ->  case head args of
                            Var varname -> if getVar varname s
                                    then return [(varname, DSU op False False)]
                                    else error $ varname ++ " not initialized before use in function " ++ name
                            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu ++ dsus

step dsus (If cond t1 t2) = do
    dsuCond <- generateContextDSU [cond]
    oldState <- get 

    dsuT1 <- generateContextDSU [t1]
    stateT1 <- get 

    put oldState

    dsuT2 <- generateContextDSU  [t2]
    stateT2 <- get 

    put $ AS (getStateFunNames stateT1) (getStateContext stateT1 `union` getStateContext stateT2)
    return $ dsus ++ concat [dsuCond, dsuT1, dsuT2]

step dsus _ = return dsus
