module AllStructures where

import Random
import Structures
import Data.List

ll = DS "Linked List"       [(Op InsertArgVal       ((LinLog 1 0), N)),
                            (Op InsertVal           ((LinLog 0 0), N)),
                            (Op DeleteByArg         ((LinLog 1 0), N)),
                            (Op DeleteByVal         ((LinLog 1 0), N)),
                            (Op DeleteExtremalVal   ((LinLog 1 0), N)),
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

array = DS "Array"          [(Op InsertArgVal       ((LinLog 0 0), N)),
                            --(Op InsertVal           ((LinLog 0 0), N)),
                            (Op DeleteByArg         ((LinLog 0 0), N)),
                            (Op DeleteByVal         ((LinLog 1 0), N)),
                            (Op DeleteExtremalVal   ((LinLog 1 0), N)),
                            (Op DeleteExtremalArg   ((LinLog 0 0), N)),
                            (Op ExtremalArg         ((LinLog 0 0), N)),
                            (Op ExtremalVal         ((LinLog 0 0), N)),
                            (Op FindByVal           ((LinLog 1 0), N)),
                            (Op FindByArg           ((LinLog 0 0), N)),
                            (Op Map                 ((LinLog 1 0), N)),
                            (Op Size                ((LinLog 0 0), N)),
                            (Op Empty               ((LinLog 0 0), N)),
                            (Op BoundByArg          ((LinLog 1 0), N)),
                            (Op BoundByVal          ((LinLog 1 0), N))]

allStructures :: [Structure]
allStructures = [rbt, heap, hash, ll, array]

recommendDS :: [OperationName] -> IO Structure
recommendDS opns =  do  let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures 
                        let  bestStructures = head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
                        ridx <- randomRIO (0, length bestStructures - 1)
                        return $ bestStructures !! ridx

recommendAllDS :: [OperationName] -> [Structure]
recommendAllDS opns =  let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures 
                        in  head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
 
