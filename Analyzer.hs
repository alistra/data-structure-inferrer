module Analyzer where

import Defs.Structures
import Defs.AST
import Data.List
import Control.Arrow

data DSInfo =   DSI { getDSIVarName :: Name,
                    isStatic :: Bool,
                    getUses :: [DSUse] } deriving (Show, Eq)

data DSUse =    DSU {  getDSUOpName :: OperationName,
                    isHeavilyUsed :: Bool, 
                    isUserDependent :: Bool }
                | DSUInit deriving (Show, Eq)

isDeclaredStructure t dsis = t `elem` map getDSIVarName dsis 

setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True

isStaticDSU _ = True -- stub

analyze :: [Term] -> [DSInfo]
analyze = generateDSI . generateDSU  

generateDSI :: [(Name, DSUse)] -> [DSInfo]
generateDSI allDSU@((name, _):dsus) = let (sameName, otherNames) = partition (\x -> fst x == name) allDSU in
    generateSingleDSI sameName : generateDSI otherNames
generateDSI [] = []

generateSingleDSI :: [(Name, DSUse)] -> DSInfo
generateSingleDSI allNDSU@((name, _):_) = DSI name static cleanDSU where
    mergeDSU allDSU@(dsu:dsus) = let (sameOp, otherOps) = partition (\x -> getDSUOpName x == getDSUOpName dsu) allDSU in
        mergeSingleDSU sameOp : mergeDSU otherOps
    mergeSingleDSU = foldl1 (\(DSU name1 hu1 ud1) (DSU name2 hu2 ud2) -> DSU name1 (hu1 || hu2) (ud1 || ud2))
    cleanDSU = mergeDSU (map snd allNDSU)
    static = isStaticDSU cleanDSU

generateDSU :: [Term] -> [(Name, DSUse)]
generateDSU = concatMap stepDSU 

stepDSU :: Term -> [(Name, DSUse)]
stepDSU (Block body) = generateDSU body
stepDSU (DSInit name) = [(name, DSUInit)]
stepDSU (While cond body) = map (Control.Arrow.second setHeavyUsage) $ generateDSU [cond,body]
stepDSU (Funcall name args) = case head args of
    Var varname -> [(varname, DSU opname False False)] where
        opname = case name of
            "insert"        -> InsertVal
            "max"           -> ExtremalVal
            "update"        -> UpdateByVal
            "delete_max"    -> DeleteExtremalVal
    _ -> []
stepDSU (If cond t1 t2) = generateDSU [cond,t1,t2]
stepDSU _ = []
