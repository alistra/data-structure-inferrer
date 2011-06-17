module Typechecker where

import Defs.Common
import Defs.AST
import Control.Monad.State
{-
type TypeContext = [(Name, Type)]
type Typechecker a = State TypeContext a

typecheckP :: [Term] -> Bool

typecheckT (And t1 t2) = 
typecheckT (Assign n t1) =
typecheckT (Block ts) =
typecheckT (Dec n) =
typecheckT (Div t1 t2) =
typecheckT (Eq t1 t2) =
typecheckT (For t1 t2 t3 t4) =
typecheckT (Funcall n ts) =
typecheckT (Geq t1 t2) =
typecheckT (Gt t1 t2) =
typecheckT (If t1 t2 t3) =
typecheckT (Inc n) =
typecheckT (Int n) =
typecheckT (InitAssign n t tp) = 
typecheckT (Leq t1 t2) =
typecheckT (Lt t1 t2) =
typecheckT (Mul t1 t2) =
typecheckT (Not t1) =
typecheckT (Or t1 t2) =
typecheckT (Sub t1 t2) =
typecheckT (Sum t1 t2) =
typecheckT (While t1 t2) =
typecheckT (Var n) =
typecheckT (VarInit n tp) =
-}
