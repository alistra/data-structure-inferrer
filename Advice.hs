-- | Module providing helpful tips about minimizing the complexity of the chosen data structure
module Advice (printAdviceFromAnalysis) where

import Defs.Util
import Defs.Structures

import Analyzer
import Recommend

import Data.List

-- | Type for advice data
data Advice = Advice {
    advisedDS :: Structure,                 -- ^ Structure to be advised
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
adviceDS' n opns = let
    recOrig = recommendAllDs opns
    opnsSeqs = sequencesOfLen opns (genericLength opns - n)
    recSeqs = concatMap (\sq -> map (\x -> Advice x sq opns) $ filter (\ds-> betterThanEach ds recOrig sq) (recommendAllDs sq)) opnsSeqs
    in filterAdviceForRecommended recOrig recSeqs

-- | Returns advice data for operations @opns@
adviceDS :: [OperationName] -> [Advice]
adviceDS = adviceDS' 1

-- | Pretty prints effects of 'adviceDS''
printAdvice' :: (String -> IO ()) -> Integer -> [OperationName] -> IO ()
printAdvice' output n opns =   do
    yellowColor
    let adv = adviceDS' n opns
    let rec = recommendAllDs opns
    if length rec == 1
        then do
            output "Currently, the recommended data structure is: "
            cyanColor
            output $ (getDSName $ head rec) ++ "\n"
            yellowColor
        else do
            output "Currently, the recommended data structures are: "
            cyanColor
            output $ (foldl (\str ds -> (str ++ ", " ++ getDSName ds)) "" rec) ++ "\n"
            yellowColor
    mapM_ (printAdviceStructure output) adv
    resetColor

-- | Prints one 'Advice' element
printAdviceStructure :: (String -> IO ()) -> Advice -> IO()
printAdviceStructure output (Advice s betterOpns opns) = do
    output  "You could use "
    greenColor
    output $ getDSName s
    yellowColor
    output ", if you removed the following operations:\n"
    redColor
    output $ concatMap (\opn -> "* " ++ show opn ++ "\n") (opns \\ betterOpns)
    resetColor

-- | Pretty prints effects of 'adviceDS'
printAdvice :: (String -> IO ()) -> [OperationName] -> IO ()
printAdvice output = printAdvice' output 1

-- | Pretty print advice for a single 'DSInfo'
printDSIAdvice :: (String -> IO ()) -> DSInfo -> IO ()
printDSIAdvice output dsi = do
    let opns = map getDSUName $ getDSIDSU dsi
    printAdvice output opns

-- | Pretty printer for the advisor effects
printAdviceFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO ()
printAdviceFromAnalysis output = mapM_ (printDSIAdvice output)
