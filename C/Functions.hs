module C.Functions (dsinfFunctions) where

import Defs.Structures
import Analyzer
import Defs.Common

import Language.C
import Language.C.Data.Ident
import Language.C.Data.Position

pos :: Position
pos = position 0 "" 0 0

ni :: NodeInfo
ni = NodeInfo pos (pos,0) (Name 0)

void :: CTypeSpec
void = CVoidType ni

int :: CTypeSpec
int = CIntType ni

ds :: CTypeSpec
ds = CTypeDef (Ident "ds" 0 ni) ni

dsElem :: CTypeSpec
dsElem = CTypeDef (Ident "dselem" 0 ni) ni

-- | Basic functions for data-structure access
dsinfFunctions :: [(FunctionDeclaration CTypeSpec, [OperationName])]
dsinfFunctions = [
    (FunDecl (F "update_d")     void   [(V "ds", ds), (V "oldval", int), (V "newval", int)], [UpdateByRef, FindByVal]),
    (FunDecl (F "insert_d")     dsElem [(V "ds", ds), (V "elem", int)]                     , [InsertVal]),
    (FunDecl (F "delete_d")     void   [(V "ds", ds), (V "elem", int)]                     , [DeleteByRef, FindByVal]),
    (FunDecl (F "max_d")        dsElem [(V "ds", ds)]                                      , [ExtremalVal]),
    (FunDecl (F "min_d")        dsElem [(V "ds", ds)]                                      , [ExtremalVal]), --FIXME Minmax problem
    (FunDecl (F "delete_max_d") void   [(V "ds", ds)]                                      , [DeleteExtremalVal]),
    (FunDecl (F "delete_min_d") void   [(V "ds", ds)]                                      , [DeleteExtremalVal]),
    (FunDecl (F "search_d")     dsElem [(V "ds", ds), (V "elem", int)]                     , [FindByVal]),
    (FunDecl (F "update_de")    void   [(V "elem", dsElem), (V "newval", int)]             , [UpdateByRef]),
    (FunDecl (F "delete_de")    void   [(V "elem", dsElem)]                                , [DeleteByRef])
    ]
