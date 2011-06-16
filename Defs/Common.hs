module Defs.Common where

import System.Console.ANSI
import Data.List

-- | Changes the color of the terminal output to green
greenColor :: IO()
greenColor = setSGR [SetColor Foreground Vivid Green]
-- | Changes the color of the terminal output to yellow
yellowColor :: IO()
yellowColor = setSGR [SetColor Foreground Dull Yellow]
-- | Changes the color of the terminal output to red
redColor :: IO()
redColor = setSGR [SetColor Foreground Vivid Red]
-- | Changes the color of the terminal output to cyan
cyanColor :: IO()
cyanColor = setSGR [SetColor Foreground Vivid Cyan]
-- | Resets the color of the terminal output
resetColor :: IO()
resetColor = setSGR [Reset]

-- | Type for names
type Name =  String

-- | Count the number of occurences of given element in the list
countElem :: Eq a => a -> [a] -> Integer
countElem y xs = genericLength $ filter (== y) xs

-- | All subsequences of given length @n@ and longer of given sequence @xs@
sequencesOfLen :: Eq a =>  [a] -> Integer -> [[a]]
sequencesOfLen xs n = filter (\s -> genericLength s >= n && (s /= xs)) $ subsequences xs

-- | A function inspired by python's string.split().  A list is split
-- on a separator which is itself a list (not a single element).
split :: Eq a => [a] -> [a] -> [[a]]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ [] = Nothing
          sp1 t s = case find (t `isSuffixOf`) $ inits s of
                      Nothing -> Just (s, [])
                      Just p -> Just (take (length p - length t) p,
                                      drop (length p) s)
