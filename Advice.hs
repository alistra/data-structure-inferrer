module Advice where

import Data.List
import Structures
import AllStructures
import Recommend

sequencesOfLen :: Eq a =>  [a] -> Integer -> [[a]]
sequencesOfLen xs n = filter (\s -> (toEnum.length $ s) >= n && (s /= xs)) $ subsequences xs


notWorse :: Structure -> Structure -> [OperationName] -> Bool
notWorse s1 s2 opns = compareDS s1 s2 opns /= LT

better :: Structure -> Structure -> [OperationName] -> Bool
better s1 s2 opns   | s1 == s2 = True
                    | otherwise = compareDS s1 s2 opns == GT

betterThanEach :: Structure -> [Structure] -> [OperationName] -> Bool
betterThanEach s1 ss opns = all (\s2-> better s1 s2 opns) ss 

adviceDS' :: Integer -> [OperationName] -> [([OperationName], Structure)]
adviceDS' n opns =  let recOrig = recommendAllDs opns
                        opnsSeqs = sequencesOfLen opns (toEnum (length opns) - n)
                        recSeqs = concatMap (\seq -> map (\x -> (seq,x)) $ filter (\ds-> betterThanEach ds recOrig seq) (recommendAllDs seq)) opnsSeqs 
                            in recSeqs -- \\ recOrig


--adviceDS :: [OperationName] -> [Structure]
--adviceDS = adviceDS' 1

--printAdvice' :: Integer -> [OperationName] -> IO () --add the colors of the rainbow
--printAdvice' n opns =   let adv = adviceDS' n opns 
--                            in mapM_ (printAdviceStructure opns) adv

--printAdviceStructure :: [OperationName] -> Structure -> IO() --change to worse operations not only not existing ones
--printAdviceStructure opns s = putStr $    "You could use " ++
--                                            getDSName s ++
--                                           ", if you removed the following operations:\n" ++ 
--                                            concatMap (\opn -> "* " ++ show opn ++ "\n") (opns \\ map getOpName (getDSOps s))
                                    

--printAdvice :: [OperationName] -> IO ()
--printAdvice = printAdvice' 1
