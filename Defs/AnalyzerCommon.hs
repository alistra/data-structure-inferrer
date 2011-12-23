module Defs.AnalyzerCommon where

-- | Basic functions for data-structure access
dsinfFunctions :: [Function]
dsinfFunctions = [
    Function (F "update")     Nothing        [(V "ds", Ds), (V "oldval", TInt), (V "newval", TInt)]       (Block []),
    Function (F "insert")     (Just DsElem)  [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "delete")     Nothing        [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "max")        (Just DsElem)  [(V "ds", Ds)]                                               (Block []),
    Function (F "min")        (Just DsElem)  [(V "ds", Ds)]                                               (Block []),
    Function (F "delete_max") Nothing        [(V "ds", Ds)]                                               (Block []),
    Function (F "search")     (Just DsElem)  [(V "ds", Ds), (V "elem", TInt)]                             (Block []),
    Function (F "update")     Nothing        [(V "elem", DsElem), (V "newval", TInt)]                      (Block []),
    Function (F "delete")     Nothing        [(V "elem", DsElem)]                                          (Block [])
    ]

-- | Functions that are easilly decomposed into 'dsinfFunctions' functions
dsinfAliasFunctions :: [Function]
dsinfAliasFunctions = []
