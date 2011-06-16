-- | Module that provides helpful tips for the programmer, that can minimize the complexity of the chosen data structure
module Advice where

import Defs.Common
import Defs.Structures

import AllStructures
import Recommend

import Data.List

data AdviceData = Advice { advisedDS :: Structure, reducedOperations :: [OperationName], operations :: [OperationName] } deriving Show

-- | Checks if structure @s1@ is not worse than structure @s2@ on operations @opns@
notWorse :: Structure -> Structure -> [OperationName] -> Bool
notWorse s1 s2 opns = compareDS s1 s2 opns /= LT


-- | Checks if the structure @s1@ is better than the structure @s2@ on operations @opns@
better :: Structure -> Structure -> [OperationName] -> Bool
better s1 s2 opns   | s1 == s2 = True
                    | otherwise = compareDS s1 s2 opns == GT
-- | Checks if the structure @s1@ is better than each of the structures in @ss@ on operations @opns@
betterThanEach :: Structure -> [Structure] -> [OperationName] -> Bool
betterThanEach s1 ss opns = all (\s2-> better s1 s2 opns) ss 

-- | Removes already recommended data structures @recs@ from the advised structures
filterAdviceDataForRecommended :: [Structure] -> [AdviceData] -> [AdviceData]
filterAdviceDataForRecommended recs = filter (\(Advice ds _ _) -> ds `notElem` recs)

-- | Returns advice data for operations @opns@ and at most @n@ operations to forget about
adviceDS' :: Integer -> [OperationName] -> [AdviceData]
adviceDS' n opns =  let recOrig = recommendAllDs opns
                        opnsSeqs = sequencesOfLen opns (integerLength opns - n)
                        recSeqs = concatMap (\seq -> map (\x -> Advice x seq opns) $ filter (\ds-> betterThanEach ds recOrig seq) (recommendAllDs seq)) opnsSeqs 
                            in filterAdviceDataForRecommended recOrig recSeqs

-- | Returns advice data for operations @opns@
adviceDS :: [OperationName] -> [AdviceData]
adviceDS = adviceDS' 1

-- | Pretty prints effects of 'adviceDS''
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
                    putStrLn (foldl (\str ds -> (str ++ ", " ++ getDSName ds)) "" rec)
                    yellowColor
    mapM_ printAdviceStructure adv
    resetColor

-- | Prints one 'AdviceData' element
printAdviceStructure :: AdviceData -> IO()
printAdviceStructure (Advice s seq opns) = do
    putStr  "You could use " 
    greenColor
    putStr $ getDSName s
    yellowColor
    putStrLn ", if you removed the following operations:"
    redColor
    putStr $ concatMap (\opn -> "* " ++ show opn ++ "\n") (opns \\ seq)
    resetColor

-- | Pretty prints effects of 'adviceDS'
printAdvice :: [OperationName] -> IO ()
printAdvice = printAdvice' 1
