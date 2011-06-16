-- | Module for adding possible structures and functions modifying the structures
module AllStructures where

import Defs.Structures

import Data.List
import Data.Maybe

-- | Function that adds extremal element cache to a data structure
extremalElemCache :: Structure -> Structure 
extremalElemCache (DS name ops) = DS (name ++ " with extreme element caching") ops' where
    extVal = fromJust $ find (\dsop -> getOpName dsop == ExtremalVal) ops
    delByRef = fromJust $ find (\dsop -> getOpName dsop == DeleteByRef) ops
    ops' = [Op DeleteByRef (max (getComplexity extVal) (getComplexity delByRef)),
            Op ExtremalVal (LinLog 0 0, N)] ++ 
            filter (\dsop -> getOpName dsop `notElem` [ExtremalVal, DeleteByRef]) ops

-- | Function that links the elements of a data structure
linkedLeaves :: Structure -> Structure 
linkedLeaves (DS name ops) = DS (name ++ " with linked leaves") ops' where
    bndByRef = fromJust $ find (\dsop -> getOpName dsop == BoundByRef) ops
    insVal = fromJust $ find (\dsop -> getOpName dsop == InsertVal) ops
    findByVal = fromJust $ find (\dsop -> getOpName dsop == InsertVal) ops
    ops' = [Op BoundByRef (LinLog 0 0, N),
            Op InsertVal (max (getComplexity bndByRef) (getComplexity insVal))] ++
            filter (\dsop -> getOpName dsop `notElem` [InsertVal, BoundByRef]) ops

{-
                            Op BoundByRef
                            Op DecreaseValByRef
                            Op DeleteByRef
                            Op DeleteExtremalVal
                            Op Difference
                            Op Empty 
                            Op ExtremalVal
                            Op FindByVal
                            Op InsertVal
                            Op Intersection
                            Op Map
                            Op Size
                            Op SymDifference
                            Op Union
                            Op UpdateByRef
-}

-- | Linked list
ll :: Structure
ll = DS "Linked List"       [
                            Op BoundByRef           (LinLog 1 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N), 
                            Op Difference           (LinLog 2 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 0, N),
                            Op Intersection         (LinLog 2 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 2 0, N),
                            Op Union                (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N)
                                                                    ]
-- | Red Black Trees 
rbt :: Structure
rbt = DS "Red-Black Trees"  [
                            Op BoundByRef           (LinLog 0 1, N),
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op DeleteByRef          (LinLog 0 1, N),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op Difference           (LinLog 1 1, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op FindByVal            (LinLog 0 1, N),
                            Op InsertVal            (LinLog 0 1, N),
                            Op Intersection         (LinLog 1 1, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 1, N),
                            Op Union                (LinLog 1 1, N),
                            Op UpdateByRef          (LinLog 0 1, N)
                                                                    ]
-- | Hashtable                                                      
hash :: Structure
hash = DS "Hashtable"       [
                            Op BoundByRef           (LinLog 1 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N),
                            Op Difference           (LinLog 1 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 0, AE),
                            Op Intersection         (LinLog 1 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 0, N),
                            Op Union                (LinLog 1 0, AE),
                            Op UpdateByRef          (LinLog 0 0, N)
                                                                    ]
-- | Heap
heap :: Structure
heap = DS "Heap"            [
                            Op BoundByRef           (LinLog 0 0, N),
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op DeleteByRef          (LinLog 0 1, N),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op Difference           (LinLog 1 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 1, N),
                            Op Intersection         (LinLog 1 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 0, N),
                            Op Union                (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N)
                                                                    ]
{-
binom = DS "Binomial Heap"  [
                            Op BoundByRef
                            Op DeleteByRef          
                            Op Difference
                            Op FindByVal            (LinLog 1 0, N),
                            Op Intersection
                            Op SymDifference        
                            Op UpdateByRef
                            Op InsertVal            (LinLog 0 0, A),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op Union                (LinLog 0 1, N),
                            Op Empty                (LinLog 0 0, N)
                                                                    ]

fibo = DS "Fibonacci Heap"  [
                            Op BoundByRef
                            Op DeleteByRef
                            Op Difference
                            Op FindByVal
                            Op Intersection
                            Op SymDifference
                            Op UpdateByRef
                            Op InsertVal           (LinLog 0 0, A),
                            Op DeleteExtremalVal   (LinLog 0 1, N),
                            Op DecreaseValByRef    (LinLog 0 0, N),
                            Op ExtremalVal         (LinLog 0 0, N),
                            Op Map                 (LinLog 1 0, N),
                            Op Size                (LinLog 0 0, N),
                            Op Union               (LinLog 0 0, A),
                            Op Empty               (LinLog 0 0, N)
                                                                    ]

array = DS "Array"          [
                            Op BoundByRef           (LinLog 0 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N),
                            Op Difference           (LinLog 2 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op Intersection         (LinLog 2 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 2 0, N),
                            Op Union                (LinLog 1 0, N),
                            Op UpdateByRef          (LinLog 0 0, N),
                                                                    ]
-}

-- | List of all possible structures
allStructures :: [Structure]
allStructures = [rbt, hash, heap, ll] ++
                map extremalElemCache [rbt, hash, ll] ++
                map linkedLeaves [rbt] --, binom, array, fibo]
