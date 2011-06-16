module Analyzer where

import Defs.Structures
import Defs.Common
import Defs.AST

import Recommend

import Data.List
import Control.Arrow
import Control.Monad.State

-- | Data structure analysis info type
data DSInfo =   DSI { getDSIVarName :: Name,    -- ^ Variable holding the data structure
                    isStatic :: Bool,           -- ^ Is the variable static or dynamic
                    getUses :: [DSUse]          -- ^ Data structure use cases
                    } deriving (Show, Eq)

-- | Data structure use case type
data DSUse =    DSU {getDSUOpName :: OperationName, -- ^ Operation used
                    isHeavilyUsed :: Bool,          -- ^ Is it heavily used
                    isUserDependent :: Bool         -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
                    } deriving (Show, Eq)

-- | List of pairs: variable name, use case
type AnalyzerOutput = [(Name, DSUse)]

-- | State monad with 'Context' state returning 'AnalyzerOutput'
type Analyzer = State [Name] AnalyzerOutput

-- | Analyzer context containing variable names with data structures
type Context = [Name]

setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: [DSInfo] -> IO()
printRecommendationFromAnalysis = mapM_ printDSI 

-- | Pretty print single 'DSInfo' 
printDSI dsi = do
    putStr "The recommended structure for "
    redColor
    putStr $ getDSIVarName dsi
    resetColor 
    putStrLn " is:"
    cyanColor
    recommendedDS >>= print
    resetColor where
        recommendedDS = do 
            let opns = map getDSUOpName $ getUses dsi
            recommendDS opns

-- | Run the analyzer
analyze :: [Term] -> [DSInfo]
analyze = generateDSI . generateDSU 

-- | Generate 'DSInfo's for each data structure in the program
generateDSI :: AnalyzerOutput -> [DSInfo]
generateDSI allDSU@((name, _):dsus) = let (sameName, otherNames) = partition (\x -> fst x == name) allDSU in
    generateSingleDSI sameName : generateDSI otherNames
generateDSI [] = []

-- | Generate single 'DSInfo' from 'DSUse's of on data structure
generateSingleDSI :: AnalyzerOutput -> DSInfo
generateSingleDSI allNDSU@((name, _):_) = DSI name static cleanDSU where
    mergeDSU allDSU@(dsu:dsus) = let (sameOp, otherOps) = partition (\x -> getDSUOpName x == getDSUOpName dsu) allDSU in
        mergeSingleDSU sameOp : mergeDSU otherOps
    mergeDSU [] = []
    mergeSingleDSU = foldl1 (\(DSU name1 hu1 ud1) (DSU name2 hu2 ud2) -> DSU name1 (hu1 || hu2) (ud1 || ud2))
    cleanDSU = mergeDSU (map snd allNDSU)
    static = True

-- | Start the state monad to gather 'DSUse's from the AST
generateDSU :: [Term] -> AnalyzerOutput
generateDSU ts = evalState (foldlTerms step [] ts) []

-- | Analyze the terms using the state monad
generateContextDSU :: [Term] -> Analyzer
generateContextDSU = foldlTerms step [] where

-- | Foldl 'Term's using the 'step' function to generate 'AnalyzerOutput'
foldlTerms :: (AnalyzerOutput -> Term -> State Context AnalyzerOutput) -> AnalyzerOutput -> [Term] -> State Context AnalyzerOutput 
foldlTerms f start [] = return start
foldlTerms f start (r:rest) = do 
    dsus <- f start r
    foldlTerms f dsus rest

-- | Folding step generating 'DSUse's
step :: AnalyzerOutput -> Term -> Analyzer

step dsus (Block body) = do
    newDSU <- generateContextDSU body
    return $ dsus ++ newDSU

step dsus (DSInit name) = do
    ctx <- get
    if name `elem` ctx 
        then error $ name ++ " already initialized"
        else put (name:ctx) >> return dsus 

step dsus (While cond body) = do
    newDSU <- generateContextDSU [cond,body] 
    return $ dsus ++ map (Control.Arrow.second setHeavyUsage) newDSU

step dsus (Funcall name args) = do 
    let opname = case name of
            "insert"        -> Just InsertVal
            "find"          -> Just FindByVal
            "update"        -> Just UpdateByRef -- FIXME
            "max"           -> Just ExtremalVal
            "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing

    argDsus <- generateContextDSU args

    funcallDsu <- case opname of
            Nothing ->  return []
            Just op ->  case head args of
                            Var varname -> do
                                ctx <- get
                                if varname `elem` ctx 
                                    then return  [(varname, DSU op False False)]
                                    else error $ varname ++ " not initialized before use in function " ++ name
                            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu ++ dsus

step dsus (If cond t1 t2) = do
    dsuCond <- generateContextDSU [cond]
    oldCtx <- get

    dsuT1 <- generateContextDSU [t1]
    ctxT1 <- get

    put oldCtx

    dsuT2 <- generateContextDSU  [t2]
    ctxT2 <- get

    put $ union ctxT1 ctxT2
    return $ dsus ++ concat [dsuCond, dsuT1, dsuT2]

step dsus _ = return dsus
