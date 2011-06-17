-- | Module with Abstract Syntax Tree syntax
module Defs.AST where

import Defs.Common

-- | Type for language operations
data Term = And Term Term               -- ^ Logical and
            | Assign Name Term          -- ^ Variable assignment
            | Block [Term]              -- ^ Block of operations
            | Dec Name                  -- ^ Decrement
            | Div Term Term             -- ^ Division
            | Eq Term Term              -- ^ Equality test
            | For Term Term Term Term   -- ^ For loop
            | Funcall Name [Term]       -- ^ Function call
            | Geq Term Term             -- ^ Greater or equal test
            | Gt Term Term              -- ^ Greater than test
            | If Term Term Term         -- ^ If-else statement
            | Inc Name                  -- ^ Increment
            | Int Int                   -- ^ Integer Constant
            | InitAssign Name Term Type -- ^ Variable declaration and assignment
            | Leq Term Term             -- ^ Less or equal test
            | Lt Term Term              -- ^ Less than thest
            | Mul Term Term             -- ^ Multiplication
            | Not Term                  -- ^ Logical not
            | Or Term Term              -- ^ Logical or
            | Record [(Name, Term)]     -- ^ Record
            | Sub Term Term             -- ^ Subtraction
            | Sum Term Term             -- ^ Addition
            | While Term Term           -- ^ While loop
            | Var Name                  -- ^ Variable
            | VarInit Name Type         -- ^ Variable declaration
            deriving (Show, Eq)

-- | Type for the language values
data Type = TInt                    -- ^ Integer
            | TBool                 -- ^ Boolean
            | Ds                    -- ^ Data structure reference
            | DsElem                -- ^ Data structure element reference
            | TRec [(Name, Type)]   -- ^ Record
            deriving (Show, Eq)
