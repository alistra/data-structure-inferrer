module Defs.Common where
-- | Type for names
type Name = String

-- | Type for storing the variable names defined in a program
newtype VariableName = V { unV :: String } deriving (Show, Eq)
-- | Type for storing the function names defined in a program
newtype FunctionName = F { unF :: String } deriving (Show, Eq)

