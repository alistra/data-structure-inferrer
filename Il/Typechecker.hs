module Il.Typechecker where

import Defs.Common
import Il.AST

import Control.Monad.State
import Data.Maybe

-- | Variables with types, return value for a function, other declared functions
data TypeState = TS {
    getStateReturn :: Maybe Type,
    getStateFunctions :: [Function],
    getStateVariables :: [(VariableName, Type)] }

-- | State monad to remember the 'TypeState'
type Typechecker a = State TypeState a

-- | Typecheck a term block
typecheckB :: [Term] -> Typechecker [Maybe Type]
typecheckB = mapM typecheckT

-- | Assert a type to a term, raise an error otherwise
assertType :: Term -> Type -> Typechecker ()
assertType t1 tp2 = do
    tp1' <- typecheckT t1
    case tp1' of
        Just tp1 -> when (tp1 /= tp2) (error $ "Type error: " ++ show t1 ++ " is type " ++ show tp1 ++ ", should be" ++ show tp2)
        Nothing -> error $ "Type error: " ++ show t1 ++ " does not return a value, should return " ++ show tp2

-- | Typecheck a function definition
typecheckF :: [Function] ->  Function -> [Maybe Type]
typecheckF fns (Function _ tp args (Block body)) = evalState (typecheckB body) (TS tp (dsinfFunctions ++ fns) args)
typecheckF _ (Function name _ _ _) = error $ "Function definition for " ++ show name ++ " is not a code block"

-- | Typecheck a function call
typecheckFC :: FunctionName -> [Term] -> Typechecker (Maybe Type)
typecheckFC f ts = do
    s <- get
    tps <- typecheckB ts
    let fns = filter (\x -> getFunName x == f && map fromJust tps == map snd (getFunArgs x)) (getStateFunctions s) --FIXME add safe lookup
    case fns of
        [] -> error $ "No matching function: " ++ show f
        fn:[]   -> return $ getFunType fn
        fn:fnss -> error $ "More than one function matching: " ++ show (fn:fnss)

-- | Typecheck one field of a record
typecheckR :: (String, Term) -> Typechecker (String, Type)
typecheckR (n, t) = do
    tp <- typecheckT t
    case tp of
        Nothing -> error $ "Type error: " ++ show t ++ " returning no value in a record field"
        Just tp1 -> return (n, tp1)

-- | Typecheck a Dec or Inc operation
typecheckIncDec :: VariableName -> Typechecker (Maybe Type)
typecheckIncDec v = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just TInt -> return $ Just TInt
        Just tp -> error $ "Type error: " ++ show v ++ " has type " ++ show tp ++ "instead of " ++ show TInt
        Nothing -> error $ "Type error: " ++ show v ++ " is not initilized, should be initialized as " ++ show TInt

-- | Typecheck a term, 'Nothing' symbolizes the expressions without a value
typecheckT :: Term -> Typechecker (Maybe Type)
typecheckT (And t1 t2) = logOp t1 t2

typecheckT (Assign v t1) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp -> do
            assertType t1 tp
            return Nothing
        Nothing -> error $ "Variable " ++ show v ++ " not initialized"

typecheckT (Block ts) = do
    _ <- typecheckB ts
    return Nothing

typecheckT (Inc v) = typecheckIncDec v
typecheckT (Dec v) = typecheckIncDec v


typecheckT (Div t1 t2) = mathOp t1 t2

typecheckT (Eq t1 t2) = relOp t1 t2

typecheckT (For t1 t2 t3 t4) = do
    _ <- typecheckT t1
    assertType t2 TBool
    _ <- typecheckT t3
    _ <- typecheckT t4
    return Nothing

typecheckT (Funcall f ts) = typecheckFC f ts

typecheckT (Geq t1 t2) = relOp t1 t2

typecheckT (Gt t1 t2) = relOp t1 t2

typecheckT (If t1 t2 t3) = do
    assertType t1 TBool
    _ <- typecheckT t2
    _ <- typecheckT t3
    return Nothing

typecheckT (Int _) = return $ Just TInt

typecheckT (InitAssign v t tp) = do
    assertType t tp
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp1 -> error $ "Variable " ++ show v ++ " already initialized with type " ++ show tp1
        Nothing -> do
            put $ TS (getStateReturn s) (getStateFunctions s) ((v,tp):tcx)
            return Nothing

typecheckT (Leq t1 t2) = relOp t1 t2

typecheckT (Lt t1 t2) = relOp t1 t2

typecheckT (Mul t1 t2) = mathOp t1 t2

typecheckT (Not t1) = do
    assertType t1 TBool
    return $ Just TBool

typecheckT (Or t1 t2) = logOp t1 t2

typecheckT (Record rs) = fmap (Just . TRec) (mapM typecheckR rs)

typecheckT (Return t) = do
    s <- get
    let rt = fromJust $ getStateReturn s
    assertType t rt
    return Nothing

typecheckT (Sub t1 t2) = mathOp t1 t2

typecheckT (Sum t1 t2) = mathOp t1 t2

typecheckT (While t1 t2) = do
    assertType t1 TBool
    _ <- typecheckT t2
    return Nothing

typecheckT (Var v) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Nothing -> error $ "Variable " ++ show v ++ " not initialized"
        Just tp -> return $ Just tp

typecheckT (VarInit v tp) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp1 -> error $ "Variable " ++ show v ++ " already initialized with type " ++ show tp1
        Nothing -> do
            put $ TS (getStateReturn s) (getStateFunctions s) ((v,tp):tcx)
            return Nothing

relOp :: Term -> Term -> Typechecker (Maybe Type)
relOp t1 t2 = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

mathOp :: Term -> Term -> Typechecker (Maybe Type)
mathOp t1 t2 = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TInt

logOp :: Term -> Term -> Typechecker (Maybe Type)
logOp t1 t2 = do
    assertType t1 TBool
    assertType t2 TBool
    return $ Just TBool
