module Defs.Structures where

import Data.Ord
import Defs.Util

-- | Data structure for keeping data structures
data Structure = DS {   getDSName :: String, -- ^ name of the data structure
                        getDSOps :: [DSOperation] -- ^ operations along with their complexities
                        } deriving Eq

instance Show Structure where
    show = getDSName

-- | Type for operation names
data OperationName =  InsertVal         -- ^ Insert an element
                    | DeleteByRef       -- ^ Delete the element
                    | FindByVal         -- ^ Find the element by value
                    | UpdateByRef       -- ^ Update the value
                    | DeleteExtremalVal -- ^ Delete the extreme value
                    | ExtremalVal       -- ^ Maximum or minimum
                    | BoundByRef        -- ^ Precedessor or successor
                    | DecreaseValByRef  -- ^ Update that decreases the value
                    | Union             -- ^ Union
                    | Intersection      -- ^ Intersection
                    | Difference        -- ^ Difference
                    | SymDifference     -- ^ Symmetric difference
                    | Map               -- ^ Map elements
                    | Size              -- ^ Checking the size
                    | Empty             -- ^ Checking the empiness
                    deriving (Show, Eq)

-- | Additional complexity qualifiers
data ComplexityCharacteristics = AE -- ^ Amortized expected time
                                | E -- ^ Expected time
                                | A -- ^ Amortized time
                                | N -- ^ Normal time
                                deriving (Ord, Eq, Show)

-- | Full complexity type
type Complexity = (AsymptoticalComplexity, ComplexityCharacteristics)

-- | Asymptotical complexity type, remembered as the exponent of the @n@ and the number of stacked logarithms
data AsymptoticalComplexity = LinLog {  getLin :: Integer, -- ^ exponent of @n@
                                        getLog :: Integer  -- ^ number of the stacked logarithms
                                        } deriving (Eq)

instance Ord AsymptoticalComplexity where
    compare (LinLog l1 l2) (LinLog r1 r2) = case compare l1 r1 of
        EQ -> compare l2 r2
        x -> x

instance Show AsymptoticalComplexity where
    show (LinLog 0 0)   = "O(1)"
    show (LinLog 1 0)   = "O(n)"
    show (LinLog 0 n)   = "O("   ++ logs n ++ " n)"
    show (LinLog 1 n)   = "O(n " ++ logs n ++ " n)"
    show (LinLog n 0)   = "O(n^" ++ show n ++ ")"
    show (LinLog n m)   = "O(n^" ++ show n ++ " " ++ logs m ++ " n)"

-- | Function to pretty print stacked logarithms
logs :: Integer -> String
logs 0 = ""
logs n = "log" ++ logs (n-1)

-- | Type for operation and its complexity
data DSOperation = Op { getOpName :: OperationName, -- ^ Operation name
                        getComplexity :: Complexity -- ^ Complexity of the operation
                        } deriving (Show, Eq)

instance Ord DSOperation where
    compare (Op _ c1) (Op _ c2) = compare c1 c2

-- | Function to check which of the two data structures, on given operations, is better
compareDS ::  Structure -> Structure -> [OperationName] -> Ordering
compareDS s1 s2 opns = let  ops1 = filter (\x -> getOpName x `elem` opns) (getDSOps s1)
                            ops2 = filter (\x -> getOpName x `elem` opns) (getDSOps s2) in
                                case Data.Ord.comparing length ops1 ops2 of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> if null ops1
                                        then EQ
                                        else case Data.Ord.comparing maximum ops1 ops2 of
                                            LT -> GT
                                            GT -> LT
                                            EQ -> let ordList = zipWith compare ops1 ops2 in
                                                compare (countElem GT ordList) (countElem LT ordList)
