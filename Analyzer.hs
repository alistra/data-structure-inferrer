module Analyzer where

import Data.Monoid
import Data.List
import Control.Monad.State

import Defs.Util
import Defs.Common
import Defs.Structures

import Advice
import Recommend

-- | Data structure use case
data DSUse = DSU {
    getDSUName      :: OperationName,                         -- ^ Operation used
    isHeavilyUsed   :: Bool,                                  -- ^ Is it heavily used
    isUserDependent :: Bool                                   -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
    } deriving (Show, Eq)

-- | Data structure for analysis info for one data-structure (possibly in many forms of different variables in functions)
data DSInfo = DSI {
    getDSINames :: [(FunctionName, VariableName)],           -- ^ Variables, used in functions, holding the analyzed data structure
    getDSIDSU   :: [DSUse]                                   -- ^ 'DSUse's of the data structure
    } deriving (Show, Eq)

instance Monoid DSInfo where
    mempty = DSI [] []
    mappend (DSI n1 d1) (DSI n2 d2) = DSI (n1 `union` n2) (d1 `union` d2)

-- | Function call - name of the function, relevant arguments
type FunctionCall = (FunctionName, [Maybe VariableName])

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

-- | Term analyzer basic output, variables and 'DSUse's
type Output = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateCalls    :: [FunctionCall]                        -- ^ 'FunctionCall's gathered through the analysis
    } deriving (Show)

instance Monoid TermAnalyzerState where
    mempty = AS []
    mappend (AS cs1) (AS cs2) = AS (cs1 `union` cs2)

-- | Pretty print single 'DSInfo'
printDSI :: (String -> IO ()) -> DSInfo -> IO ()
printDSI output dsi = do
    output "The recommended structure for "
    redColor
    output $ show (getDSINames dsi) ++ "\n"
    resetColor
    output " is:"
    cyanColor
    recommendedDS >>= output.show
    resetColor where
        recommendedDS = do
            let opns = map getDSUName $ getDSIDSU dsi
            recommendDS opns

-- | Pretty print advice for a single 'DSInfo'
printDSIAdvice :: (String -> IO ()) -> DSInfo -> IO ()
printDSIAdvice output dsi = do
    let opns = map getDSUName $ getDSIDSU dsi
    printAdvice output opns

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO()
printRecommendationFromAnalysis output = mapM_ (printDSI output)

-- | Pretty printer for the advisor effects
printAdviceFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO ()
printAdviceFromAnalysis output = mapM_ (printDSIAdvice output)

-- | Stupid merging of dsis --TODO remove this function, rewrite analyzeFunctions correctly
stupidMerge ::  [DSInfo] -> [DSInfo]
stupidMerge (dsi:dsis) = let (same, different) = partition (\dsi' -> getDSINames dsi `intersect` getDSINames dsi' /= []) dsis in
    mconcat (dsi:same) : stupidMerge different
stupidMerge [] = []
