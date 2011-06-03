module Analyzer where

import Defs.Structures
import Defs.Common
import Defs.AST

import Recommend

import Data.List
import Control.Arrow
import Control.Monad.State

data DSInfo =   DSI { getDSIVarName :: Name,
                    isStatic :: Bool,
                    getUses :: [DSUse] } deriving (Show, Eq)

data DSUse =    DSU {  getDSUOpName :: OperationName,
                    isHeavilyUsed :: Bool, 
                    isUserDependent :: Bool } deriving (Show, Eq)

type AnalyzerOutput = [(Name, DSUse)]
type Analyzer = State [Name] AnalyzerOutput

setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True

isStaticDSU _ = True -- stub



printRecommendationFromAnalysis :: [DSInfo] -> IO()
printRecommendationFromAnalysis = mapM_ printDSI 

printDSI dsi = do
    putStr "The recommended structure for "
    redColor
    putStr $ getDSIVarName dsi
    resetColor 
    putStrLn " is:"
    cyanColor
    recommendedDS >>= putStrLn.show
    resetColor where
        recommendedDS = do 
            let opns = map getDSUOpName $ getUses dsi
            recommendDS opns

analyze :: [Term] -> [DSInfo]
analyze = generateDSI . generateDSU 



generateDSI :: AnalyzerOutput -> [DSInfo]
generateDSI allDSU@((name, _):dsus) = let (sameName, otherNames) = partition (\x -> fst x == name) allDSU in
    generateSingleDSI sameName : generateDSI otherNames
generateDSI [] = []

generateSingleDSI :: AnalyzerOutput -> DSInfo
generateSingleDSI allNDSU@((name, _):_) = DSI name static cleanDSU where
    mergeDSU allDSU@(dsu:dsus) = let (sameOp, otherOps) = partition (\x -> getDSUOpName x == getDSUOpName dsu) allDSU in
        mergeSingleDSU sameOp : mergeDSU otherOps
    mergeDSU [] = []
    mergeSingleDSU = foldl1 (\(DSU name1 hu1 ud1) (DSU name2 hu2 ud2) -> DSU name1 (hu1 || hu2) (ud1 || ud2))
    cleanDSU = mergeDSU (map snd allNDSU)
    static = isStaticDSU cleanDSU



generateDSU :: [Term] -> AnalyzerOutput
generateDSU ts = evalState (foldlTerms step [] ts) []

generateContextDSU :: [Term] -> Analyzer
generateContextDSU = foldlTerms step []

foldlTerms :: (AnalyzerOutput -> Term -> State Context AnalyzerOutput) -> AnalyzerOutput -> [Term] -> State Context AnalyzerOutput 
foldlTerms f start [] = return start
foldlTerms f start (r:rest) = do 
    dsus <- f start r
    foldlTerms f dsus rest

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

step dsus (Funcall name args) = do --parse funcalls in arguments
    let opname = case name of
            "insert"        -> Just InsertVal
            "max"           -> Just ExtremalVal
            "update"        -> Just UpdateByVal
            "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing

    case opname of
            Nothing ->  return dsus
            Just op ->  case head args of
                            Var varname -> do
                                ctx <- get
                                if varname `elem` ctx 
                                    then return $ dsus ++ [(varname, DSU op False False)]
                                    else error $ varname ++ " not initialized before use in function " ++ name
                            _           -> error "Not implemented yet"

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
