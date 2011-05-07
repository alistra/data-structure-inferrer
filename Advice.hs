module Advice where

import Data.List
import Structures
import AllStructures
import Recommend
import System.Console.ANSI

greenColor = setSGR [SetColor Foreground Vivid Green]
yellowColor = setSGR [SetColor Foreground Dull Yellow]
redColor = setSGR [SetColor Foreground Vivid Red]
cyanColor = setSGR [SetColor Foreground Vivid Cyan]
resetColor = setSGR [Reset]

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
                            in filter (\(_, ds) -> ds `notElem` recOrig) recSeqs


adviceDS :: [OperationName] -> [([OperationName], Structure)]
adviceDS = adviceDS' 1

printAdvice' :: Integer -> [OperationName] -> IO ()
printAdvice' n opns =   do    
                            yellowColor
                            let adv = adviceDS' n opns 
                            let rec = recommendAllDs opns
                            if length rec == 1
                                then    do  
                                            putStr "Currently recommended data structure is: " 
                                            cyanColor
                                            putStrLn (getDSName $ head rec)
                                            yellowColor
                                else    do
                                            putStr "Currently recommended data structures are: "
                                            cyanColor
                                            putStrLn (foldl (\str ds -> ( str ++ ", " ++ getDSName ds)) "" rec)
                                            yellowColor
                            mapM_ (\(seq, ds) -> printAdviceStructure ds seq opns) adv
                            resetColor

printAdviceStructure :: Structure -> [OperationName] -> [OperationName] -> IO()
printAdviceStructure s seq opns = do
                                        putStr  "You could use " 
                                        greenColor
                                        putStr $ getDSName s
                                        yellowColor
                                        putStrLn ", if you removed the following operations:"
                                        redColor
                                        putStr $ concatMap (\opn -> "* " ++ show opn ++ "\n") (opns \\ seq)
                                        resetColor

printAdvice :: [OperationName] -> IO ()
printAdvice = printAdvice' 1
