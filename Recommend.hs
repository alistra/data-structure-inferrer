module Recommend where

import Structures
import AllStructures
import Data.List
import Random

recommendDS :: [OperationName] -> IO Structure
recommendDS opns =  do  let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures 
                        let bestStructures = head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
                        ridx <- randomRIO (0, length bestStructures - 1)
                        return $ bestStructures !! ridx

recommendAllDs :: [OperationName] -> [Structure]
recommendAllDs opns = recommendAllDsFromList opns allStructures

recommendAllDsFromList opns structs =    let sorted = reverse $ sortBy (\x y-> compareDS x y opns) structs
                                            in head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
