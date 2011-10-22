-- | Module for recommending a data structure based on operations used
module Recommend
  ( recommendDS,
    recommendAllDs ) where

import Defs.Structures

import AllStructures

import Data.List
import Random

-- | Recommends a data structure which is best for given operations @opns@. If there's more than one optimal structure, it chooses one at random
recommendDS :: [OperationName] -> IO Structure
recommendDS opns =  do
    let sorted = reverse $ sortBy (\x y-> compareDS x y opns) allStructures
    let bestStructures = head $ groupBy (\x y -> compareDS x y opns == EQ) sorted
    ridx <- randomRIO (0, length bestStructures - 1)
    return $ bestStructures !! ridx

-- | Recommends all optimal data structures for given operations @opns@
recommendAllDs :: [OperationName] -> [Structure]
recommendAllDs opns = recommendAllDsFromList opns allStructures

-- | Recommends the best possible data structure from @structs@ for given operations @opns@
recommendAllDsFromList :: [OperationName] -> [Structure] -> [Structure]
recommendAllDsFromList opns structs = let
     sorted = reverse $ sortBy (\x y-> compareDS x y opns) structs
     in head $ groupBy (\x y -> compareDS x y opns == EQ) sorted
