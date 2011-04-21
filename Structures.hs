import Data.List

data Structure = DS { getDSName :: String, getDSOps :: [DSOperation] }

instance Show Structure where
    show = getDSName

data OperationName = InsertArgVal
                    | InsertVal
                    | DeleteByArg
                    | DeleteByVal
                    | FindByVal
                    | FindByArg
                    | DeleteExtremalVal
                    | DeleteExtremalArg
                    | ExtremalArg
                    | ExtremalVal
                    | BoundByArg
                    | BoundByVal
                    | Map
                    | Size
                    | Empty deriving Eq
                    
-- what with function that gets arg/val

data ComplexityCharacteristics = AE | E | A | N deriving (Ord, Eq)

type Complexity = (AsymptoticalComplexity, ComplexityCharacteristics)

data AsymptoticalComplexity = LinLog Integer Integer deriving (Eq)

instance Ord AsymptoticalComplexity where
    compare (LinLog l1 l2) (LinLog r1 r2) = case compare l1 r1 of
        EQ -> compare l2 r2
        x -> x

instance Show AsymptoticalComplexity where
    show (LinLog 0 0)   = "O(1)"
    show (LinLog 1 0)   = "O(n)"
    show (LinLog 0 n)   = "O(" ++ (logs n) ++ " n)" 
    show (LinLog 1 n)   = "O(n " ++ (logs n) ++ " n)" 
    show (LinLog n 0)   = "O(n^" ++ (show n) ++ ")" 
    show (LinLog n m)   = "O(n^" ++ (show n) ++ " " ++ (logs m) ++ " n)"

logs :: Integer -> String
logs 0 = ""
logs n = "log" ++ (logs (n-1))


data DSOperation = Op { getOpName :: OperationName, getComplexity :: Complexity } deriving Eq

instance Ord DSOperation where
    compare (Op _ c1) (Op _ c2) = compare c1 c2


ll = DS "Linked List"       [(Op InsertArgVal       ((LinLog 1 0), N)),
                            (Op InsertVal           ((LinLog 0 0), N)),
                            (Op DeleteByArg         ((LinLog 1 0), N)),
                            (Op DeleteByVal         ((LinLog 1 0), N)),
                            (Op DeleteExtremalVal   ((LinLog 0 0), N)),
                            (Op DeleteExtremalArg   ((LinLog 0 0), N)),
                            (Op ExtremalArg         ((LinLog 0 0), N)),
                            (Op ExtremalVal         ((LinLog 0 0), N)),
                            (Op FindByVal           ((LinLog 1 0), N)),
                            (Op FindByArg           ((LinLog 1 0), N)),
                            (Op Map                 ((LinLog 1 0), N)),
                            (Op Size                ((LinLog 0 0), N)),
                            (Op Empty               ((LinLog 0 0), N)),
                            (Op BoundByArg          ((LinLog 1 0), N)),
                            (Op BoundByVal          ((LinLog 1 0), N))]

rbt = DS "Red-Black Trees"  --[(Op InsertArgVal     ((LinLog 1 0), N)),
                            [(Op InsertVal          ((LinLog 0 1), N)),
                            --(Op DeleteByArg       ((LinLog 1 0), N)),
                            (Op DeleteByVal         ((LinLog 0 1), N)),
                            (Op DeleteExtremalVal   ((LinLog 0 1), N)),
                            --(Op DeleteExtremalArg ((LinLog 0 1), N)),
                            --(Op ExtremalArg       ((LinLog 0 0), N)),
                            (Op ExtremalVal         ((LinLog 0 0), N)),
                            (Op FindByVal           ((LinLog 0 1), N)),
                            --(Op FindByArg         ((LinLog 1 0), N)),
                            (Op Map                 ((LinLog 1 0), N)),
                            (Op Size                ((LinLog 0 0), N)),
                            (Op Empty               ((LinLog 0 0), N)),
                            --(Op BoundByArg        ((LinLog 0 0), N)),
                            (Op BoundByVal          ((LinLog 0 1), N))]

                            --[(Op InsertArgVal     ((LinLog 0 0), AE),
hash = DS "Hashtable"       [(Op InsertArgVal       ((LinLog 0 0), N)),
                            --(Op DeleteByArg       ((LinLog 1 0), N)),
                            (Op DeleteByArg         ((LinLog 0 0), N)),
                            --(Op DeleteExtremalVal ((LinLog 0 0), N)),
                            --(Op DeleteExtremalArg ((LinLog 0 0), N)),
                            --(Op ExtremalArg       ((LinLog 0 0), N)),
                            --(Op ExtremalVal       ((LinLog 0 0), N)),
                            --(Op FindByVal         ((LinLog 0 0), N)),
                            (Op FindByArg           ((LinLog 0 0), N)),
                            --(Op Map               ((LinLog 1 0), N)),
                            (Op Size                ((LinLog 0 0), N)),
                            (Op Empty               ((LinLog 0 0), N))]
                            --(Op BoundByArg        ((LinLog 0 0), N)),
                            --(Op BoundByVal        ((LinLog 1 0), N))]

                            --(Op InsertArgVal      ((LinLog 0 1), N)),
heap = DS "Heap"            [(Op InsertVal          ((LinLog 0 1), N)),
                            --(Op DeleteByArg       ((LinLog 1 0), N)),
                            (Op DeleteByVal         ((LinLog 1 0), N)),
                            (Op DeleteExtremalVal   ((LinLog 0 1), N)),
                            --(Op DeleteExtremalArg ((LinLog 0 0), N)),
                            --(Op ExtremalArg       ((LinLog 0 0), N)),
                            (Op ExtremalVal         ((LinLog 0 0), N)),
                            (Op FindByVal           ((LinLog 1 0), N)),
                            --(Op FindByArg         ((LinLog 1 0), N)),
                            (Op Map                 ((LinLog 1 0), N)),
                            (Op Size                ((LinLog 0 0), N)),
                            (Op Empty               ((LinLog 0 0), N)),
                            --(Op BoundByArg        ((LinLog 0 0), N)),
                            (Op BoundByVal          ((LinLog 1 0), N))]


countElem :: Eq a => a -> [a] -> Integer
countElem _ [] = 0
countElem y (x:xs)  | y == x = 1 + (countElem y xs)
                    | otherwise = (countElem y xs)

compareDS ::  Structure -> Structure -> [OperationName] -> Ordering
compareDS s1 s2 opns = let  ops1 = filter (\x -> elem (getOpName x) opns) (getDSOps s1)
                            ops2 = filter (\x -> elem (getOpName x) opns) (getDSOps s2) in
                                case compare (length ops1) (length ops2) of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> case compare (maximum ops1) (maximum ops2) of --penis empyy list
                                        LT -> GT
                                        GT -> LT
                                        EQ -> let ordList = zipWith (compare) ops1 ops2 in
                                            compare (countElem LT ordList) (countElem GT ordList)
-- check the worst case complexity, and if the same then count better ones

allStructures :: [Structure]
allStructures = [rbt, heap, hash, ll]

