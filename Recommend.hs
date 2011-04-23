module Recommend where

import Structures
import AllStructures
import Data.List
import Random

recommendDS :: [OperationName] -> IO Structure
recommendDS opns =  do  let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures 
                        let  bestStructures = head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
                        ridx <- randomRIO (0, length bestStructures - 1)
                        return $ bestStructures !! ridx

recommendAllDS :: [OperationName] -> [Structure]
recommendAllDS opns =  let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures 
                        in  head $ groupBy (\x y -> compareDS x y opns == EQ) sorted 
 
