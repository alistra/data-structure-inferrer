module Main (main) where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Control.Monad

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optAdvice     :: Bool
                        , optRecommend  :: Bool
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optAdvice     = False
                        , optRecommend  = False
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg (\arg opt -> return opt { optInput = readFile arg }) "FILE")
        "Input file"

    , Option "o" ["output"]
        (ReqArg (\arg opt -> return opt { optOutput = writeFile arg }) "FILE")
        "Output file"

    , Option "s" ["string"]
        (ReqArg (\arg opt -> return opt { optInput = return arg }) "FILE")
        "Input string"

    , Option "a" ["advice"]
        (NoArg  (\opt -> return opt { optAdvice = True }))
        "Give advice about the data structure in the code"

    , Option "r" ["recommend"]
        (NoArg  (\opt -> return opt { optRecommend = True }))
        "Give recommendations about the data structure in the code"

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
                        , optOutput = output } = opts
         
            when verbose (hPutStrLn stderr "Command line options loaded")
         
            input >>= output
        (_, nonOptions, errors) -> do
            unless (errors == []) (putStrLn "Command line errors:")
            mapM_ (\s -> putStrLn ("\t" ++ s)) errors
            unless (nonOptions == []) (putStrLn "Command line non-options present:")
            mapM_ (\s -> putStrLn ("\t" ++ s)) nonOptions
            exitFailure

