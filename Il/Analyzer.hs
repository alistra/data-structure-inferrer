{-# OPTIONS_GHC -fno-warn-unused-binds #-} --TODO

module Il.Analyzer (
    printRecommendationFromAnalysis,
    printAdviceFromAnalysis,
    analyze,
    ) where

import Defs.Structures
import Defs.Common
import Defs.AST

import Analyzer hiding (getFunName, getFunArgs, getFunType)

import Data.Monoid
import Control.Monad.State

-- | Runs everything that is needed to analyze a program
analyze :: [Function] -> [DSInfo]
analyze functions = let dsfs = map generateDSF functions in
    stupidMerge $ analyzeFunctions dsfs

-- | Start the state monad to create a 'DSFun' for function
generateDSF :: Function -> DSFun Type
generateDSF fn = let (dsus, st) = runState (sumTerms step [getFunBody fn]) (AS []) in
    let fd = FunDecl (getFunName fn) (getFunType fn) (getFunArgs fn) in
    DSF fd (getStateCalls st) (generateDSI (getFunName fn) dsus)

-- | Analyze a block of terms using the state monad
stepBlock :: [Term] -> TermAnalyzer Output
stepBlock = sumTerms step

-- | Sums the 'DSUse's using the 'step' function and concating the results
sumTerms :: (Term -> TermAnalyzer Output) -> [Term] -> TermAnalyzer Output
sumTerms f ts = fmap concat (mapM f ts)

-- | Function putting a function call in the state
putCall :: FunctionName -> [Term] -> TermAnalyzer ()
putCall name args = do
    let cleanArgs = map justifyVars args
    let call = (name, cleanArgs)
    modify  $ \s -> s {getStateCalls = call:getStateCalls s} where
        justifyVars :: Term -> Maybe VariableName
        justifyVars (Var v) = Just v -- FIXME should work on function calls returning dses, not only vars
        justifyVars _ = Nothing

-- | Generate 'DSUse's for a single 'Term'
step :: Term -> TermAnalyzer Output

step (Block body) = stepBlock body

step (VarInit _ Ds) = return []
step (InitAssign _ _ Ds) = return []

step (While cond body) = stepBlock [cond,body]

step (Funcall name args) = do
    let opname = case name of -- FIXME nicer with usage of dsinfFunctions from Common
            F "insert"        -> Just InsertVal
            F "find"          -> Just FindByVal
            F "update"        -> Just UpdateByRef
            F "max"           -> Just ExtremalVal
            F "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing
                                            -- FIXME add reading the function calls
    argDsus <- stepBlock args

    funcallDsu <- case opname of
        Nothing ->  do
            putCall name args
            return []
        Just op ->  case head args of       -- FIXME dsinfFunctions ds argument recognition
            Var varname -> return [(varname, DSU op False False)]
            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu

step (If cond t1 t2) = do
    dsuCond <- step cond
    oldState <- get

    dsuT1 <- step t1
    stateT1 <- get

    put oldState

    dsuT2 <- step t2
    stateT2 <- get

    put (stateT1 `mappend` stateT2)
    return $ concat [dsuCond, dsuT1, dsuT2]

-- Dummy steps
step t = case t of
    Var _ -> return []
    VarInit _ _ -> return []
    Inc _ -> return []
    Dec _ -> return []
    Int _ -> return []

    Assign _ t -> step t
    Lt t1 t2 -> stepBlock [t1, t2]
    Mul t1 t2 -> stepBlock [t1, t2]
    s -> error $ "No step for " ++ show s
