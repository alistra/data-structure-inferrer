module Typechecker where

import Defs.Common
import Defs.AST

import Control.Monad.State
import Data.Maybe

-- | Variables with types and return value for a function
data TypeState = TS { getStateReturn :: Type, getStateVariables :: [(Name, Type)] }

-- | State monad to remember the 'TypeState'
type Typechecker a = State TypeState a

-- | Runs the typechecker on the program 
typecheckP :: [Function] -> [[Maybe Type]]
typecheckP fns = map typecheckF fns

-- | Typecheck a term block
typecheckB :: [Term] -> Typechecker [Maybe Type]
typecheckB = mapM typecheckT
  
-- | Assert a type to a term, raise an error otherwise
assertType :: Term -> Type -> Typechecker ()
assertType t1 tp2 = do
    tp1' <- typecheckT t1
    case tp1' of
        Just tp1 ->  if tp1 /= tp2
            then error $ "Type error: " ++ (show t1) ++ " is type " ++ (show tp1) ++ ", should be" ++ (show tp2)
            else return ()
        Nothing -> error $ "Type error: " ++ (show t1) ++ " does not return a value, should return " ++ (show tp2)

-- | Typecheck a function definition
typecheckF :: Function -> [Maybe Type]
typecheckF (FunDef name tp args (Block body)) = evalState (typecheckB body) (TS tp args)

-- | Typecheck a function call
typecheckFC :: Name -> [Term] -> Typechecker (Maybe Type)
typecheckFC f [] = return Nothing
typecheckFC f ts = do
    tps <- typecheckB ts
    case (f, fromJust $ head tps) of
        -- Ds operations
        ("update", Ds) -> return Nothing
        ("insert", Ds) -> return $ Just DsElem
        ("delete", Ds) -> return Nothing
        ("max", Ds) -> return $ Just DsElem
        ("min", Ds) -> return $ Just DsElem
        ("delete_max", Ds) -> return $ Just DsElem
        -- DsElem operations
        ("update", DsElem) -> return Nothing
        ("delete", DsElem) -> return Nothing
        (_, _) -> return Nothing

-- | Typecheck one field of the record
typecheckR :: (Name, Term) -> Typechecker (Name, Type)
typecheckR (n, t) = do
    tp <- typecheckT t
    case tp of
        Nothing -> error $ "Type error: " ++ (show t) ++ " returning no value in a record field"
        Just tp1 -> return (n, tp1)

-- | Typecheck a term, 'Nothing' symbolizes the expressions without a value
typecheckT :: Term -> Typechecker (Maybe Type)
typecheckT (And t1 t2) = do
    assertType t1 TBool
    assertType t2 TBool
    return $ Just TBool

typecheckT (Assign v t1) = do
    s <- get 
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp -> do
            assertType t1 tp
            return Nothing
        Nothing -> error $ "Variable " ++ v ++ " not initialized"

typecheckT (Block ts) = do
    typecheckB ts
    return Nothing

typecheckT (Dec v) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just TInt -> return $ Just TInt
        Just tp | tp /= TInt -> error $ "Type error: " ++ v ++ " has type " ++ (show tp) ++ "instead of " ++ (show TInt)
        Nothing -> error $ "Type error: " ++ v ++ " is not initilized, should be initialized as " ++ (show TInt)

typecheckT (Div t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TInt

typecheckT (Eq t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

typecheckT (For t1 t2 t3 t4) = do
    typecheckT t1
    assertType t2 TBool
    typecheckT t3
    typecheckT t4
    return Nothing

typecheckT (Funcall f ts) = typecheckFC f ts
    
typecheckT (Geq t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

typecheckT (Gt t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

typecheckT (If t1 t2 t3) = do
    assertType t1 TBool
    typecheckT t2
    typecheckT t3
    return Nothing

typecheckT (Inc v) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just TInt -> return $ Just TInt
        Just tp | tp /= TInt -> error $ "Type error: " ++ v ++ " has type " ++ (show tp) ++ "instead of " ++ (show TInt)
        Nothing -> error $ "Type error: " ++ v ++ " is not initilized, should be initialized as " ++ (show TInt)

typecheckT (Int n) = return $ Just TInt

typecheckT (InitAssign v t tp) = do
    assertType t tp
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp1 -> error $ "Variable " ++ v ++ " already initialized with type " ++ (show tp1)
        Nothing -> do
            put $ TS (getStateReturn s) ((v,tp):tcx)
            return Nothing

typecheckT (Leq t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

typecheckT (Lt t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TBool

typecheckT (Mul t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TInt

typecheckT (Not t1) = do
    assertType t1 TBool
    return $ Just TBool

typecheckT (Or t1 t2) = do
    assertType t1 TBool
    assertType t2 TBool
    return $ Just TBool

typecheckT (Record rs) = do
    mapM typecheckR rs >>= return . Just . TRec

typecheckT (Return t) = do
    s <- get
    let rt = getStateReturn s
    assertType t rt
    return Nothing

typecheckT (Sub t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TInt

typecheckT (Sum t1 t2) = do
    assertType t1 TInt
    assertType t2 TInt
    return $ Just TInt

typecheckT (While t1 t2) = do
    assertType t1 TBool
    typecheckT t2
    return Nothing

typecheckT (Var v) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Nothing -> error $ "Variable " ++ v ++ " not initialized"
        Just tp -> return $ Just tp
    
typecheckT (VarInit v tp) = do
    s <- get
    let tcx = getStateVariables s
    case lookup v tcx of
        Just tp -> error $ "Variable " ++ v ++ " already initialized with type " ++ (show tp)
        Nothing -> do
            put $ TS (getStateReturn s) ((v,tp):tcx)
            return Nothing
    
