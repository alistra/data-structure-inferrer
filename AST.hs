module AST where
import Structures

type Name = String
data Term = And Term Term
            | Assign Name Term
            | Block [Term] 
            | Dec Term
            | Div Term Term
            | DSInit Name
            | Eq Term Term
            | For Term Term Term Term 
            | Funcall Name [Term]
            | Geq Term Term
            | Gt Term Term
            | If Term Term Term
            | Inc Term
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
