module AllStructures where

import Defs.Structures

import Data.List
import Data.Maybe

extremalElemCache :: Structure -> Structure 
extremalElemCache (DS name ops) = DS (name ++ " with extreme element caching") ops' where
    extVal = fromJust $ find (\dsop -> getOpName dsop == ExtremalVal) ops
    delByVal = fromJust $ find (\dsop -> getOpName dsop == DeleteByVal) ops
    delByRef = fromJust $ find (\dsop -> getOpName dsop == DeleteByRef) ops
    ops' = [Op DeleteByRef (max (getComplexity extVal) (getComplexity delByRef)),
            Op DeleteByVal (max (getComplexity extVal) (getComplexity delByVal)),
            Op ExtremalVal (LinLog 0 0, N)] ++ filter (\dsop -> getOpName dsop `notElem` [ExtremalVal, DeleteByVal, DeleteByRef]) ops

{-
                            Op BoundByRef
                            Op BoundByVal
                            Op DecreaseValByRef
                            Op DeleteByRef
                            Op DeleteByVal
                            Op DeleteExtremalVal
                            Op Difference
                            Op Empty 
                            Op ExtremalVal
                            Op FindByVal
                            Op InsertVal
                            Op Intersect
                            Op Map
                            Op Size
                            Op SymDifference
                            Op Union
                            Op UpdateByRef
                            Op UpdateByVal
-}

ll = DS "Linked List"       [
                            Op BoundByRef           (LinLog 1 0, N),
                            Op BoundByVal           (LinLog 1 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteByVal          (LinLog 1 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N), 
                            Op Difference           (LinLog 2 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 0, N),
                            Op Intersect            (LinLog 2 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 2 0, N),
                            Op Union                (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N),
                            Op UpdateByVal          (LinLog 1 0, N)
                                                                    ]

rbt = DS "Red-Black Trees"  [
                            Op BoundByRef           (LinLog 0 1, N),
                            Op BoundByVal           (LinLog 0 1, N),
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op DeleteByRef          (LinLog 0 1, N),
                            Op DeleteByVal          (LinLog 0 1, N),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op Difference           (LinLog 1 1, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op FindByVal            (LinLog 0 1, N),
                            Op InsertVal            (LinLog 0 1, N),
                            Op Intersect            (LinLog 1 1, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 1, N),
                            Op Union                (LinLog 1 1, N),
                            Op UpdateByRef          (LinLog 0 1, N),
                            Op UpdateByVal          (LinLog 0 1, N)
                                                                    ]

hash = DS "Hashtable"       [
                            Op BoundByRef           (LinLog 1 0, N),
                            Op BoundByVal           (LinLog 1 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByVal          (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N),
                            Op Difference           (LinLog 1 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 1 0, N),
                            Op FindByVal            (LinLog 0 0, N),
                            Op InsertVal            (LinLog 0 0, AE),
                            Op Intersect            (LinLog 1 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 0, N),
                            Op Union                (LinLog 1 0, AE),
                            Op UpdateByVal          (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N)
                                                                    ]

heap = DS "Heap"            [
                            Op BoundByRef           (LinLog 0 0, N),
                            Op BoundByVal           (LinLog 1 0, N),
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op DeleteByVal          (LinLog 1 0, N),
                            Op DeleteByRef          (LinLog 0 1, N),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op Difference           (LinLog 1 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 1, N),
                            Op Intersect            (LinLog 1 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 1 0, N),
                            Op Union                (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N),
                            Op UpdateByVal          (LinLog 1 0, N)
                                                                    ]
{-
binom = DS "Binomial Heap"  [
                            Op BoundByRef
                            Op BoundByVal
                            Op DeleteByRef          
                            Op DeleteByVal
                            Op Difference
                            Op FindByVal            (LinLog 1 0, N),
                            Op Intersect
                            Op SymDifference        
                            Op UpdateByRef
                            Op UpdateByVal          
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
                            Op BoundByVal
                            Op DeleteByRef
                            Op DeleteByVal
                            Op Difference
                            Op FindByVal
                            Op Intersect
                            Op SymDifference
                            Op UpdateByRef
                            Op UpdateByVal
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
                            Op BoundByVal           (LinLog 0 0, N),
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteByVal          (LinLog 1 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N),
                            Op Difference           (LinLog 2 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op Intersect            (LinLog 2 0, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 2 0, N),
                            Op Union                (LinLog 1 0, N),
                            Op UpdateByRef          (LinLog 0 0, N),
                            Op UpdateByVal          (LinLog 1 0, N)
                                                                    ]
-}


allStructures :: [Structure]
allStructures = [extremalElemCache rbt,
                rbt,
                hash,
                extremalElemCache hash,
                heap,
                extremalElemCache ll,
                ll] --, binom, array, fibo]
