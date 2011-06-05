module Tests where

import Il.Lexer
import Il.Parser
import Defs.Common

import Analyzer

import System.IO
import System.Directory
import Data.List
import Prelude hiding (lex)

-- | A function inspired by python's string.split().  A list is split
-- on a separator which is itself a list (not a single element).
split :: Eq a => [a] -> [a] -> [[a]]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ [] = Nothing
          sp1 t s = case find (t `isSuffixOf`) $ inits s of
                      Nothing -> Just (s, [])
                      Just p -> Just (take (length p - length t) p,
                                      drop (length p) s)

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    allfiles <- getDirectoryContents path
    let files = filter (\s -> last (split "/" s) `notElem` [".", ".."]) allfiles
    return $ map (path++) files

openTestFile name =
    catch (openFile name ReadMode)
        (\_ -> error $ "Cannot open "++ name)
         
runTestFiles :: [String] -> IO()
runTestFiles = mapM_ runTest

runTest :: String -> IO()
runTest name = do
    yellowColor
    putStrLn $ "Test File " ++ name
    resetColor
    handle <- openTestFile name
    contents <- hGetContents handle
    test contents
    hClose handle

test = printRecommendationFromAnalysis.analyze.parse.lex

runIlTests = listFiles "Il/tests/" >>= runTestFiles
