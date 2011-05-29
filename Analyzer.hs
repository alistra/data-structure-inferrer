module Analyzer where

import Defs.Structures
import Defs.AST

data DSInfo = DSI { getDSIVarName :: Name,
                    isStatic :: Bool,
                    getUses :: [DSUse] } deriving (Show, Eq)

data DSUse = DSU {  getDSUOpName :: OperationName,
                    isHeavilyUsed :: Bool, 
                    isUserDependent :: Bool } deriving (Show, Eq)

analyze :: [Term] -> [DSInfo]
analyze = foldl step []

analyzeWithContext :: [DSInfo] -> [Term] -> [DSInfo]
analyzeWithContext context = foldl step context

isDeclaredStructure t dsis = t `elem` (map getDSIVarName dsis) 

step :: [DSInfo] -> Term -> [DSInfo]
--step dsis (Assign v (Var v1)) = if v1 `isDeclaredStructure` dsis
--    then DSI v (isStatic v1) (getUses v1):dsis 
--    else dsis
step dsis (Block b) = analyzeWithContext dsis b
step dsis (DSInit v) = if v `isDeclaredStructure` dsis
    then error $ "Already initialized " ++ (show v) 
    else DSI v True []:dsis
--step dsis (For t1 t2 t3 body) = 
--step dsis (Funcall name args) = -- two passes in a loop
--step dsis (If cond t1 t2) = 
step dsis _ = dsis
