module Defs.AST where

import Defs.Common

data Term = And Term Term
            | Assign Name Term
            | Block [Term] 
            | Dec Name
            | Div Term Term
            | DSInit Name
            | Eq Term Term
            | For Term Term Term Term 
            | Funcall Name [Term]
            | Geq Term Term
            | Gt Term Term
            | If Term Term Term
            | Inc Name
            | Int Int
            | Leq Term Term
            | Lt Term Term
            | Mul Term Term
            | Not Term
            | Or Term Term
            | Sub Term Term
            | Sum Term Term
            | While Term Term
            | Var Name
            deriving (Show, Eq)
