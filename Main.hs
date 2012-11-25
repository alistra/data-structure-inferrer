module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import Safe

--import Il.Lexer
--import Il.Parser
--import Il.Analyzer
--import Il.Typechecker
import C.Analyzer
import Analyzer
import Advice
import Recommend
import CompileDriver

import Prelude hiding (lex)

data Action = AAdvice
    | ADefaultRecommend
    | ARecommend
    | ACompile
    | AInline deriving (Eq)

instance Show Action where
    show AAdvice = "advice (-a)"
    show ADefaultRecommend = show ARecommend ++ " (default)"
    show ARecommend = "recommend (-r)"
    show ACompile = "compile (-c)"
    show AInline = "inline (-i)"

data Options = Options  { optVerbose    :: Bool
                        , optOutput     :: String -> IO ()
                        , optAction     :: Action
                        }


checkArgs :: Options -> Action -> IO ()
checkArgs (Options { optAction = ADefaultRecommend }) _ = return ()
checkArgs (Options { optAction = action }) oldAction | action == oldAction = return ()
checkArgs (Options { optAction = action }) oldAction = error $ (show action) ++ " incompatible with " ++ (show oldAction)

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "o" ["output"]
        (ReqArg (\arg opt -> writeFile arg "" >> return opt { optOutput = appendFile arg }) "file")
        "Output file"

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
                    hPutStrLn stderr (usageInfo (prg ++ " [-OPT1 [VAL1] [-OPT2 [VAL2] [...]] [-- [CCOPTS]]") options)
                    hPutStrLn stderr "CCOPTS are passed directly to the compiler"
                    exitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (actions, rest, []) -> do
            let startOptions = Options { optVerbose    = False
                                       , optOutput     = putStr
                                       , optAction     = ADefaultRecommend
                                       }
            let (files, ccopts) = break (== "--") rest
            opts <- foldl (>>=) (return startOptions) actions
            let Options { optVerbose = verbose
                        , optOutput = output
                        , optAction = action } = opts
--            contents <- readFile (head files)
--            let ast = analyzeIl.parse.lex $ contents
            if null files
                then hPutStrLn stderr "no input files" >> exitFailure
                else case action of
                    AAdvice ->           mapM_ (\f -> output (f ++ ":\n") >> analyzeC f >>= printAdviceFromAnalysis output) files
                    ADefaultRecommend -> mapM_ (\f -> output (f ++ ":\n") >> analyzeC f >>= printRecommendationFromAnalysis output) files
                    ARecommend ->        mapM_ (\f -> output (f ++ ":\n") >> analyzeC f >>= printRecommendationFromAnalysis output) files
                    ACompile -> void $ compile files ccopts
                    AInline -> hPutStrLn stderr "Not implemented yet"
            exitSuccess

        (_, nonOptions, errors) -> do
            unless (errors == []) (hPutStrLn stderr "Command line errors:")
            mapM_ (\s -> putStrLn ('\t' : s)) errors
            unless (nonOptions == []) (hPutStrLn stderr "Command line non-options present:")
            mapM_ (\s -> putStrLn ('\t' : s)) nonOptions
            exitFailure

