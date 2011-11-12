{-# OPTIONS_GHC -pgmF drift-ghc -F #-}
-- | Module with Abstract Syntax Tree syntax
module Defs.AST where

import Control.DeepSeq

import Defs.Common

-- | Type for function definitions
data Function = Function {  getFunName :: FunctionName,             -- ^ Name of the function
                            getFunType :: Maybe Type,               -- ^ Return type, Nothing when void
                            getFunArgs :: [(VariableName, Type)],   -- ^ Names and types of the function arguments
                            getFunBody :: Term }                    -- ^ Body of the function
                            deriving (Show, Eq)

-- | Type for language operations
data Term = And Term Term                           -- ^ Logical and
            | Assign VariableName Term              -- ^ Variable assignment
            | Block [Term]                          -- ^ Block of operations
            | Dec VariableName                      -- ^ Decrement
            | Div Term Term                         -- ^ Division
            | Eq Term Term                          -- ^ Equality test
            | For Term Term Term Term               -- ^ For loop
            | Funcall FunctionName [Term]           -- ^ Function call
            | Geq Term Term                         -- ^ Greater or equal test
            | Gt Term Term                          -- ^ Greater than test
            | If Term Term Term                     -- ^ If-else statement
            | Inc VariableName                      -- ^ Increment
            | Int Int                               -- ^ Integer Constant
            | InitAssign VariableName Term Type     -- ^ Variable declaration and assignment
            | Leq Term Term                         -- ^ Less or equal test
            | Lt Term Term                          -- ^ Less than thest
            | Mul Term Term                         -- ^ Multiplication
            | Not Term                              -- ^ Logical not
            | Or Term Term                          -- ^ Logical or
            | Record [(String, Term)]               -- ^ Record
            | Return Term                           -- ^ Return a value from a function
            | Sub Term Term                         -- ^ Subtraction
            | Sum Term Term                         -- ^ Addition
            | While Term Term                       -- ^ While loop
            | Var VariableName                      -- ^ Variable
            | VarInit VariableName Type             -- ^ Variable declaration
            deriving (Show, Eq)

-- | Type for the language values
{-! for Type derive : NFData !-}
data Type = TInt                    -- ^ Integer
            | TBool                 -- ^ Boolean
            | Ds                    -- ^ Data structure reference
            | DsElem                -- ^ Data structure element reference
            | TRec [(String, Type)]   -- ^ Record
            deriving (Show, Eq)

-- | Basic functions for data-structure access
dsinfFunctions :: [Function]
dsinfFunctions = [
    Function (F "update")     Nothing        [(V "ds", Ds), (V "oldval", TInt), (V "newval", TInt)]       (Block []),
    Function (F "insert")     (Just DsElem)  [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "delete")     Nothing        [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "max")        (Just DsElem)  [(V "ds", Ds)]                                               (Block []),
    Function (F "min")        (Just DsElem)  [(V "ds", Ds)]                                               (Block []),
    Function (F "delete_max") Nothing        [(V "ds", Ds)]                                               (Block []),
    Function (F "search")     (Just DsElem)  [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "update")     Nothing        [(V "elem", DsElem), (V "newval", TInt)]                      (Block []),
    Function (F "delete")     Nothing        [(V "elem", DsElem)]                                          (Block [])
    ]

-- | Functions that are easilly decomposed into 'dsinfFunctions' functions
dsinfAliasFunctions :: [Function]
dsinfAliasFunctions = []

