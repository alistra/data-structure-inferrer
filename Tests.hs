import Analyzer
import Il.Lexer
import Il.Parser
import System.IO
import Prelude hiding (lex)

openTestFile name =
    do catch (openFile name ReadMode)
             (\_ -> do error $ "Cannot open "++ name)
         
runTestFiles :: [String] -> IO()
runTestFiles = mapM_ runTest

runTest :: String -> IO()
runTest name = do
    handle <- openTestFile name
    contents <- hGetContents handle
    putStrLn . show . test $ contents
    hClose handle

test = analyze.parse.lex

runIlTests = runTestFiles $ map (\s -> "Il/tests/" ++ s ++ ".il") (map show [1..2])
