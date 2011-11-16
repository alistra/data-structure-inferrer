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

data Input = IStdin
    | IFile [FilePath]
    | IString String deriving (Eq)

instance Show Input where
    show (IFile _) = "file arguments"
    show (IString _) = "string argument (-s)"
    show (IStdin) = "standard input argument (default)"

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: Input
                        , optOutput     :: String -> IO ()
                        , optAction     :: Action
                        }


checkInput :: Options -> Input -> IO ()
checkInput (Options { optInput = files@(IFile _)}) input = error $ show input ++ " incompatible with " ++ show files
checkInput _ _ = return ()

checkArgs :: Options -> Action -> IO ()
checkArgs (Options { optAction = ADefaultRecommend }) _ = return ()
checkArgs (Options { optAction = action }) oldAction | action == oldAction = return ()
checkArgs (Options { optAction = action }) oldAction = error $ (show action) ++ " incompatible with " ++ (show oldAction)

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "o" ["output"]
        (ReqArg (\arg opt -> writeFile arg "" >> return opt { optOutput = appendFile arg }) "file")
        "Output file"

    , Option "s" ["string"]
        (ReqArg (\arg opt -> checkInput opt (IString arg) >> return opt { optInput = IString arg}) "string")
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
        (actions, files, []) -> do
            let startOptions = Options { optVerbose    = False
                                       , optInput      = if null files then IStdin else IFile files
                                       , optOutput     = putStr
                                       , optAction     = ARecommend
                                       }
            opts <- foldl (>>=) (return startOptions) actions
            let Options { optVerbose = verbose
                        , optInput = oinput
                        , optOutput = output
                        , optAction = action } = opts
            let input = case oinput of
                    IStdin -> getContents
                    IFile f -> readFile (head f) --FIXME generalize with all files
                    IString s -> return s
            s <- input
            let ast = analyze.parse.lex $ s
            case action of
                AAdvice -> printAdviceFromAnalysis output ast
                ADefaultRecommend -> printRecommendationFromAnalysis output ast
                ARecommend -> printRecommendationFromAnalysis output ast
                ACompile -> putStrLn "Not implemented yet"
                AInline -> putStrLn "Not implemented yet"
            exitSuccess

        (_, nonOptions, errors) -> do
            unless (errors == []) (putStrLn "Command line errors:")
            mapM_ (\s -> putStrLn ('\t' : s)) errors
            unless (nonOptions == []) (putStrLn "Command line non-options present:")
            mapM_ (\s -> putStrLn ('\t' : s)) nonOptions
            exitFailure

