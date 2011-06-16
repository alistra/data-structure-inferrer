-- | Module that provides helpful tips for the programmer, that can minimize the complexity of the chosen data structure
module Advice 
      ( adviceDS,
        printAdvice,
        adviceDS',
        printAdvice' ) where

import Defs.Common
import Defs.Structures

import AllStructures
import Recommend

import Data.List

-- | Type for advice data
data Advice = Advice {  advisedDS :: Structure,                 -- ^ Structure to be advised
                        reducedOperations :: [OperationName],   -- ^ Operations on which the structure is better than the recommended one
                        operations :: [OperationName]           -- ^ Original operations (we can get recommendations from them)
                                                        } deriving Show 

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
filterAdviceForRecommended :: [Structure] -> [Advice] -> [Advice]
filterAdviceForRecommended recs = filter (\(Advice ds _ _) -> ds `notElem` recs)

-- | Returns advice data for operations @opns@ and at most @n@ operations to forget about
adviceDS' :: Integer -> [OperationName] -> [Advice]
adviceDS' n opns =  let recOrig = recommendAllDs opns
                        opnsSeqs = sequencesOfLen opns (genericLength opns - n)
                        recSeqs = concatMap (\seq -> map (\x -> Advice x seq opns) $ filter (\ds-> betterThanEach ds recOrig seq) (recommendAllDs seq)) opnsSeqs 
                            in filterAdviceForRecommended recOrig recSeqs

-- | Returns advice data for operations @opns@
adviceDS :: [OperationName] -> [Advice]
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

-- | Prints one 'Advice' element
printAdviceStructure :: Advice -> IO()
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
