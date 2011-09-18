-- | Testing module
module Tests
  ( runIlTests ) where

import Il.Lexer
import Il.Parser
import Defs.Util

import Analyzer
import Typechecker

import Control.Exception
import System.IO
import System.Directory
import Prelude hiding (lex, catch)

-- | Runs all Il tests
runIlTests :: IO ()
runIlTests = listFiles "Il/tests/" >>= runTestFiles

-- | Run all test files given by filenames
runTestFiles :: [FilePath] -> IO()
runTestFiles = mapM_ runTest

-- | Opens a test file and runs the test
runTest :: FilePath -> IO()
runTest name = do
    yellowColor
    putStrLn $ "Test File " ++ name
    resetColor
    handle <- openTestFile name
    contents <- hGetContents handle
    test contents
    hClose handle

-- | Lists all filenames in a given directory
listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    allfiles <- getDirectoryContents path
    let files = filter (\s -> last (split "/" s) `notElem` [".", ".."]) allfiles
    return $ map (path++) files

-- | Wrapper for openFile adding exception catching
openTestFile :: FilePath -> IO Handle
openTestFile name = catch (openFile name ReadMode) (\e -> do print (e :: IOException)
                                                             error $ "Cannot open "++ name)
checkTest a = print a `catch` (\e -> do 
                print "Test failed"
                print (e :: IOException))

-- | Lexes, parses, analyzes and pretty prints test results
test :: String -> IO()
test src = do
    let fns = (parse.lex) src
    mapM_ (\fn -> checkTest $ typecheckF fns fn) fns
--    (printRecommendationFromAnalysis.analyze) fns
--    return ()

