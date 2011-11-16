module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Control.Monad

import Il.Lexer
import Il.Parser
import Analyzer

import Prelude hiding (lex)

data Action = AAdvice
    | ADefaultRecommend
    | ARecommend
    | ACompile
    | AInline deriving (Eq)

instance Show Action where
    show AAdvice = "advice (-a)"
    show ADefaultRecommend = show ARecommend
    show ARecommend = "recommend (-r)"
    show ACompile = "compile (-c)"
    show AInline = "inline (-i)"

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optAction     :: Action
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optAction     = ARecommend
                        }

checkArgs :: Options -> Action -> IO ()
checkArgs (Options { optAction = ADefaultRecommend }) _ = return ()
checkArgs (Options { optAction = action }) oldAction | action == oldAction = return ()
checkArgs (Options { optAction = action }) oldAction = error $ (show action) ++ " switch is incompatible with " ++ (show oldAction) ++ " switch"

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg (\arg opt -> return opt { optInput = readFile arg }) "file")
        "Input file"

    , Option "o" ["output"]
        (ReqArg (\arg opt -> return opt { optOutput = writeFile arg }) "file")
        "Output file"

    , Option "s" ["string"]
        (ReqArg (\arg opt -> return opt { optInput = return arg }) "string")
        "Input string"

    , Option "r" ["recommend"]
        (NoArg  (\opt -> checkArgs opt ARecommend >> return opt { optAction = ARecommend }))
        "Give recommendations about the data structure in the supplied code (default)"

    , Option "a" ["advice"]
        (NoArg  (\opt -> checkArgs opt AAdvice >> return opt { optAction = AAdvice }))
        "Give advice about the data structure in the supplied code"

    , Option "c" ["compile"]
        (NoArg  (\opt -> checkArgs opt ACompile >> return opt { optAction = ACompile }))
        "Compile the code with recommended structure linked"

    , Option "i" ["inline"]
        (NoArg  (\opt -> checkArgs opt AInline >> return opt { optAction = AInline }))
        "Inline the code implementing the recommended structure in the supplied code"

    , Option "v" ["verbose"]
        (NoArg  (\opt -> return opt { optVerbose = True }))
     "Enable verbose messages"

    , Option "h" ["help"]
        (NoArg  (\_ -> do
                    prg <- getProgName
                    hPutStrLn stderr (usageInfo prg options)
                    exitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (actions, [], []) -> do
            opts <- foldl (>>=) (return startOptions) actions
            let Options { optVerbose = verbose
                        , optInput = input
                        , optOutput = output
                        , optAction = action } = opts
            s <- input
            let ast = analyze.parse.lex $ s
            case action of
                AAdvice -> printAdviceFromAnalysis ast
                ADefaultRecommend -> printRecommendationFromAnalysis ast
                ARecommend -> printRecommendationFromAnalysis ast
                ACompile -> putStrLn "Not implemented yet"
                AInline -> putStrLn "Not implemented yet"
            exitSuccess

        (_, nonOptions, errors) -> do
            unless (errors == []) (putStrLn "Command line errors:")
            mapM_ (\s -> putStrLn ('\t' : s)) errors
            unless (nonOptions == []) (putStrLn "Command line non-options present:")
            mapM_ (\s -> putStrLn ('\t' : s)) nonOptions
            exitFailure

