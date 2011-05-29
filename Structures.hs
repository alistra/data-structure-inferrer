module Structures where

import Data.List
import Data.Ord

data Structure = DS { getDSName :: String, getDSOps :: [DSOperation] } deriving Eq

instance Show Structure where
    show = getDSName

data OperationName =  InsertVal
                    | DeleteByVal
                    | DeleteByRef
                    | FindByVal
                    | UpdateByVal
                    | UpdateByRef
                    | DeleteExtremalVal
                    | ExtremalVal
                    | BoundByVal
                    | BoundByRef
                    | DecreaseValByRef
                    | Union
                    | Intersect
                    | Difference
                    | SymDifference
                    | Map
                    | Size
                    | Empty deriving (Show, Eq)
                    
data ComplexityCharacteristics = AE | E | A | N deriving (Ord, Eq, Show)

type Complexity = (AsymptoticalComplexity, ComplexityCharacteristics)

data AsymptoticalComplexity = LinLog Integer Integer deriving (Eq)

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

logs :: Integer -> String
logs 0 = ""
logs n = "log" ++ logs (n-1)

data DSOperation = Op { getOpName :: OperationName, getComplexity :: Complexity } deriving (Show, Eq)

instance Ord DSOperation where
    compare (Op _ c1) (Op _ c2) = compare c1 c2

countElem :: Eq a => a -> [a] -> Integer
countElem _ [] = 0
countElem y (x:xs)  | y == x = 1 + countElem y xs
                    | otherwise = countElem y xs

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
