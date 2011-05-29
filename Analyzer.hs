module Analyzer where

import Defs.Structures
import Defs.AST

data DSInfo =   DSI { getDSIVarName :: Name,
                    isStatic :: Bool,
                    getUses :: [DSUse] } deriving (Show, Eq)

data DSUse =    DSU {  getDSUOpName :: OperationName,
                    isHeavilyUsed :: Bool, 
                    isUserDependent :: Bool }
                | DSUInit deriving (Show, Eq)

setHeavyUsage (DSU opname _ ud) = DSU opname True ud
setUserDependance (DSU opname hu _) = DSU opname hu True




analyze :: [Term] -> [DSInfo]
analyze = generateDSI . generateDSU  

generateDSI :: [(Name, DSUse)] -> [DSInfo]
generateDSI dsus = [] -- stub

generateDSU :: [Term] -> [(Name, DSUse)]
generateDSU = concatMap stepDSU 

isDeclaredStructure t dsis = t `elem` (map getDSIVarName dsis) 

stepDSU :: Term -> [(Name, DSUse)]
stepDSU (Block body) = generateDSU body
stepDSU (DSInit name) = [(name, DSUInit)]
stepDSU (While cond body) = map (\(name, dsu) -> (name, setHeavyUsage dsu)) $ generateDSU [cond,body]
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


{-
step :: [DSUse] -> Term -> [DSUse]
step dsis (Assign v (Var v1)) = 
if v `isDeclaredStructure`dsis || v1 `isDeclaredStructure` dsis
    then error "Assigning DSes not yet available" 
    else dsis
step dsis (Block b) = analyzeWithContext dsis b
step dsis (DSInit v) = if v `isDeclaredStructure` dsis
    then error $ "Already initialized " ++ (show v) 
    else DSI v True []:dsis
step dsis (While t body) = analyzeWithContext dsis [t,body,body] --because of redefintions
step dsis (Funcall name args) = 
step dsis (If cond t1 t2) = analyzeWithContext dsis [cond,t1,t2]
step dsis _ = dsis -}
