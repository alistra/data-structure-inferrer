module Advice where

import Data.List
import Structures
import AllStructures
import Recommend

sequencesOfLen :: Eq a =>  [a] -> Integer -> [[a]]
sequencesOfLen xs n = filter (\s -> (toEnum.length $ s) >= n && (s /= xs)) $ subsequences xs

adviceDS' :: Integer -> [OperationName] -> [Structure]
adviceDS' n opns =  let recOrig = recommendAllDs opns
                        opnsSeqs = sequencesOfLen opns (toEnum (length opns) - n)
                        recSeqs = concatMap (\seq-> filter (\ds-> all (\rds-> compareDS ds rds seq == GT && compareDS ds rds opns /= LT) recOrig) 
                                                            (recommendAllDs seq)) opnsSeqs 
                        in recSeqs


adviceDS :: [OperationName] -> [Structure]
adviceDS = adviceDS' 1

printAdvice' :: Integer -> [OperationName] -> IO () --add the colors of the rainbow
printAdvice' n opns =   let adv = adviceDS' n opns 
                            in mapM_ (printAdviceStructure opns) adv

printAdviceStructure :: [OperationName] -> Structure -> IO()
printAdviceStructure opns s = putStr $    "You could use " ++
                                            getDSName s ++
                                            ", if you removed the following operations:\n" ++ 
                                            concatMap (\opn -> "* " ++ show opn ++ "\n") (opns \\ map getOpName (getDSOps s))
                                    

printAdvice :: [OperationName] -> IO ()
printAdvice = printAdvice' 1
