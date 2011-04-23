module AllStructures where

import Structures

{-
Op BoundByArg
Op BoundByRef
Op BoundByVal
Op DecreaseValByRef
Op DeleteByArg
Op DeleteByRef
Op DeleteByVal
Op DeleteExtremalArg
Op DeleteExtremalVal
Op Difference
Op Empty
Op ExtremalArg
Op ExtremalVal
Op FindByArg
Op FindByRef
Op FindByVal
Op InsertArgVal
Op InsertVal
Op Intersect
Op Map
Op Size
Op SymDifference
Op Union
Op UpdateByArg
Op UpdateByRef
Op UpdateByVal
-}

ll = DS "Linked List"       [
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByArg          (LinLog 1 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op DeleteByVal          (LinLog 1 0, N),
                            Op DeleteExtremalArg    (LinLog 0 0, N),
                            Op DeleteExtremalVal    (LinLog 1 0, N),
                            Op Difference           (LinLog 2 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalArg          (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByArg            (LinLog 1 0, N),
                            Op FindByRef            (LinLog 0 0, N),
                            Op FindByVal            (LinLog 1 0, N),
                            Op InsertArgVal         (LinLog 1 0, N),
                            Op InsertVal            (LinLog 0 0, N),
                            Op Intersect            (LinLog 1 1, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op SymDifference        (LinLog 2 0, N),
                            Op Union                (LinLog 0 0, N),
                            Op UpdateByArg          (LinLog 1 0, N),
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
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByRef            (LinLog 0 0, N),
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
                            Op DecreaseValByRef     (LinLog 0 0, N),
                            Op DeleteByArg          (LinLog 0 0, N),
                            Op DeleteByRef          (LinLog 0 0, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op FindByArg            (LinLog 0 0, N),
                            Op FindByRef            (LinLog 0 0, N),
                            Op InsertArgVal         (LinLog 0 0, AE),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op Union                (LinLog 1 0, N),
                            Op UpdateByArg          (LinLog 0 0, N),
                            Op UpdateByRef          (LinLog 0 0, N)
                                                                    ]
heap = DS "Heap"            [
                            Op DecreaseValByRef     (LinLog 0 1, N),
                            Op DeleteByVal          (LinLog 1 0, N),
                            Op DeleteExtremalVal    (LinLog 0 1, N),
                            Op Empty                (LinLog 0 0, N),
                            Op ExtremalVal          (LinLog 0 0, N),
                            Op InsertVal            (LinLog 0 1, N),
                            Op Map                  (LinLog 1 0, N),
                            Op Size                 (LinLog 0 0, N),
                            Op Union                (LinLog 1 0, N)
                                                                    ]
--
binom = DS "Binomial Heap"  [Op InsertVal          (LinLog 0 0, A),
                            Op DeleteExtremalVal   (LinLog 0 1, N),
                            Op DecreaseValByRef    (LinLog 0 1, N),
                            Op ExtremalVal         (LinLog 0 0, N),
                            Op Map                 (LinLog 1 0, N),
                            Op Size                (LinLog 0 0, N),
                            Op Union               (LinLog 0 1, N),
                            Op Empty               (LinLog 0 0, N)]

fibo = DS "Fibonacci Heap"  [Op InsertVal          (LinLog 0 0, A),
                            Op DeleteExtremalVal   (LinLog 0 1, N),
                            Op DecreaseValByRef    (LinLog 0 0, N),
                            Op ExtremalVal         (LinLog 0 0, N),
                            Op Map                 (LinLog 1 0, N),
                            Op Size                (LinLog 0 0, N),
                            Op Union               (LinLog 0 0, A),
                            Op Empty               (LinLog 0 0, N)]

array = DS "Array"          [Op InsertArgVal       (LinLog 0 0, N),
                            Op DeleteByArg         (LinLog 0 0, N),
                            Op DeleteByVal         (LinLog 1 0, N),
                            Op DeleteExtremalVal   (LinLog 1 0, N),
                            Op DeleteExtremalArg   (LinLog 0 0, N),
                            Op ExtremalArg         (LinLog 0 0, N),
                            Op ExtremalVal         (LinLog 0 0, N),
                            Op FindByVal           (LinLog 1 0, N),
                            Op FindByArg           (LinLog 0 0, N),
                            Op Map                 (LinLog 1 0, N),
                            Op Size                (LinLog 0 0, N),
                            Op Empty               (LinLog 0 0, N),
                            Op BoundByArg          (LinLog 1 0, N),
                            Op BoundByVal          (LinLog 1 0, N)]

allStructures :: [Structure]
allStructures = [rbt, heap, hash, ll, binom, array, fibo]
