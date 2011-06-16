module Defs.Common where

import System.Console.ANSI
import Data.List

greenColor = setSGR [SetColor Foreground Vivid Green]
yellowColor = setSGR [SetColor Foreground Dull Yellow]
redColor = setSGR [SetColor Foreground Vivid Red]
cyanColor = setSGR [SetColor Foreground Vivid Cyan]
resetColor = setSGR [Reset]

type Name =  String
type Context = [Name]

-- | A function inspired by python's string.split().  A list is split
-- on a separator which is itself a list (not a single element).
split :: Eq a => [a] -> [a] -> [[a]]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ [] = Nothing
          sp1 t s = case find (t `isSuffixOf`) $ inits s of
                      Nothing -> Just (s, [])
                      Just p -> Just (take (length p - length t) p,
                                      drop (length p) s)
