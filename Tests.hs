-- | Testing module
module Tests
  ( runIlTests ) where

import Il.Lexer
import Il.Parser
import Defs.Util
import Defs.AST
import Analyzer
import Il.Typechecker
import Control.DeepSeq

import System.FilePath.Posix
import Control.Exception
import Data.List
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
    contents <- readFile name
    test contents

-- | Lists all filenames in a given directory
listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    allfiles <- getDirectoryContents path
    let files = sort $ filter (\s -> last (split "/" s) `notElem` [".", ".."]) allfiles
    return $ map (path </>) files

-- | Lexes, parses, analyzes and pretty prints test results
test :: String -> IO()
test src = do
    let fns = (parse.lex) src
    typechecking fns `catch` handler "\nTyping failed"
    analysis fns `catch` handler "\nAnalysis failed"

-- | Tests analysis
analysis ::  [Function] -> IO ()
analysis fns = do
    printRecommendationFromAnalysis.analyze $ fns
    greenColor
    putStrLn "Passed"
    resetColor

-- | Tests typechecking
typechecking ::  [Function] -> IO ()
typechecking fns = mapM_ (\fn -> do
    blueColor
    deepseq (typecheckF fns fn) (putStrLn "Typed")
    resetColor) fns

-- | Handles failing tests
handler ::  String -> SomeException -> IO ()
handler msg e = do
    redColor
    putStrLn msg
    print (e :: SomeException)
    resetColor
