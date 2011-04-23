module Advice where

import Data.List
import Structures
import AllStructures
import Recommend

sequencesOfLen :: [a] -> Integer -> [[a]]
sequencesOfLen xs n = filter (\s -> (toEnum.length $ s) >= n) $ subsequences xs

adviceDS' :: Integer -> [OperationName] -> [Structure]
adviceDS' n opns =  let recOrig = recommendAllDS opns
                        opnsSeqs = sequencesOfLen opns ((toEnum $ length $ opns) - n)
                        recSeqs = nub $ concatMap recommendAllDS opnsSeqs --get the best DS for a case
                        in recSeqs \\ recOrig


adviceDS :: [OperationName] -> [Structure]
adviceDS = adviceDS' $ 1

printAdvice' :: Integer -> [OperationName] -> IO () --add colors of the rainbow
printAdvice' n opns =   let adv = adviceDS' n opns 
                            in mapM_ (printAdviceStructure $ opns) adv

printAdviceStructure :: [OperationName] -> Structure -> IO()
printAdviceStructure opns s = putStr $    "You could use " ++
                                            (getDSName s) ++
                                            ", if you removed the following operations:\n" ++ 
                                            concatMap (\opn -> "* " ++ (show opn) ++ "\n") (opns \\ (map getOpName (getDSOps s)))
                                    

printAdvice :: [OperationName] -> IO ()
printAdvice = printAdvice' $ 1
