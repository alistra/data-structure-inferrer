module CAnalyzer where

import Language.C
import Language.C.System.GCC
import Language.C.Data.Ident


import Defs.Structures
import Defs.Util
import Defs.Common

import Recommend
import Advice

import Data.List
import Data.Function
import Data.Monoid
import Data.Maybe
import Data.Maybe.HT
import Data.Either
import Control.Monad.State
import Control.Arrow
import Safe

-- | Data structure for analysis info for one data-structure (possibly in many forms of different variables in functions)
data DSInfo = DSI {
    getDSINames  :: [(FunctionName, VariableName)],             -- ^ Variables, used in functions, holding the analyzed data structure
    getDSIDSU      :: [DSUse]                                   -- ^ 'DSUse's of the data structure
    } deriving (Show, Eq)

instance Monoid DSInfo where
    mempty = DSI [] []
    mappend (DSI n1 d1) (DSI n2 d2) = DSI (n1 `union` n2) (d1 `union` d2)

-- | Function call - name of the function, relevant arguments
type FunctionCall = (FunctionName, [Maybe VariableName])

-- | Data structure for analysis info of a function definition
data DSFun = DSF {
    getDSFFun   :: FunctionDeclaration,                         -- ^ Analyzed function declaration
    getDSFCalls :: [FunctionCall],                              -- ^ 'FunctionCall's from the analyzed function
    getDSFDSI   :: [DSInfo]                                     -- ^ 'DSInfo' about the variables inside of the function, at this stage are not yet ready to obtain the information
    } deriving (Show)

-- | Data structure use case
data DSUse = DSU {
    getDSUName      :: OperationName,                           -- ^ Operation used
    isHeavilyUsed   :: Bool,                                    -- ^ Is it heavily used
    isUserDependent :: Bool                                     -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
    } deriving (Show, Eq)

-- | Type for function definitions
data FunctionDeclaration = FunDecl {
    getFunName :: FunctionName,             -- ^ Name of the function
    getFunType :: CTypeSpec,                -- ^ Return type of the function
    getFunArgs :: [(VariableName, CTypeSpec)]    -- ^ Names and types of the function arguments
    } deriving (Show)

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

-- | Term analyzer basic output, variables and 'DSUse's
type Output = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateFunction :: Maybe FunctionDeclaration,              -- ^ Current function being analyzed
    getStateFunNames :: [FunctionName],                         -- ^ All the function names
    getStateVarNames :: [VariableName],                         -- ^ All the variable names --TODO are they only relevant ones? is it needed?
    getStateCalls    :: [FunctionCall]                          -- ^ 'FunctionCall's gathered through the analysis
    } deriving (Show)

append :: TermAnalyzerState -> TermAnalyzerState -> TermAnalyzerState
append (AS f1 fns1 vns1 cs1) (AS _ _ vns2 cs2) = AS f1 fns1 (vns1 `union` vns2) (cs1 `union` cs2)

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

printDSIAdvice :: (String -> IO ()) -> DSInfo -> IO ()
printDSIAdvice output dsi = do
    let opns = map getDSUName $ getDSIDSU dsi
    printAdvice output opns

-- | Pretty printer for the analyzer effects
printRecommendationFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO()
printRecommendationFromAnalysis output = mapM_ (printDSI output)

printAdviceFromAnalysis :: (String -> IO ()) -> [DSInfo] -> IO ()
printAdviceFromAnalysis output = mapM_ (printDSIAdvice output)

-- | Stupid merging of dsis --TODO remove this function, rewrite analyzeFunctions correctly
stupidMerge ::  [DSInfo] -> [DSInfo]
stupidMerge (dsi:dsis) = let (same, different) = partition (\dsi' -> getDSINames dsi `intersect` getDSINames dsi' /= []) dsis in
    mconcat (dsi:same) : stupidMerge different
stupidMerge [] = []

-- | Merges the simple 'DSInfo's based on function calls from the functions
analyzeFunctions :: [DSFun] -> [DSInfo]
analyzeFunctions dsfs = let startingDSF = lookupDSF dsfs startingFunction in
    let functions = map getDSFFun dsfs in
    let startingVars = map snd $ concatMap getDSINames $ getDSFDSI startingDSF in
    let runMain = mapMaybe (\var -> analyzeFunction functions startingDSF var []) startingVars in --update the accumulator
    concatMap (uncurry (:)) runMain where

        analyzeFunction :: [FunctionDeclaration] -> DSFun -> VariableName -> [FunctionName] -> Maybe (DSInfo, [DSInfo])
        analyzeFunction functions dsf variable accumulator = let functionName = getFunName.getDSFFun $ dsf in
            toMaybe (functionName `notElem` accumulator) (let functionCalls = getDSFCalls dsf in
                    let relevantFunctionCalls = filter (\(_, funArgs) -> Just variable `elem` funArgs) functionCalls in
                    let irrelevantFunctionCalls = functionCalls \\ relevantFunctionCalls in --TODO remodel so we also analyze those
                    let dsis = getDSFDSI dsf in
                    let thisVariableDSI = lookupDSI dsis variable functionName in
                    let otherVariablesDSIs = dsis \\ [thisVariableDSI] in
                    let variableBindings = map (\call@(funName, _) -> second (bindFuncall functions funName) call) relevantFunctionCalls in
                    let recursiveCalls = mapMaybe (\(funName, varPairs) -> (analyzeFunction functions (lookupDSF dsfs funName) (lookupJust variable varPairs) (funName:accumulator))) variableBindings in
                    let relevantRecursiveDSI = mconcat $ map fst recursiveCalls in
                    let irrelevantRecursiveDSI = concatMap snd recursiveCalls in
                    (thisVariableDSI `mappend` relevantRecursiveDSI, otherVariablesDSIs `union` irrelevantRecursiveDSI))

-- | Lookup 'DSFun' by 'FunctionName'
lookupDSF :: [DSFun] -> FunctionName -> DSFun
lookupDSF dsfs functionName = lookupJust functionName (zip (map (getFunName.getDSFFun) dsfs) dsfs)

-- | Lookup 'DSInfo' by 'FunctionName' and 'VariableName'
lookupDSI :: [DSInfo] -> VariableName -> FunctionName -> DSInfo
lookupDSI dsis variable functionName = findJust (\dsi -> (functionName, variable) `elem` getDSINames dsi) dsis

-- | Returns pairs of local variables bound to variables in a function that is called
bindFuncall :: [FunctionDeclaration] -> FunctionName -> [Maybe VariableName] -> [(VariableName, VariableName)]
bindFuncall functions functionName vns = let
    function = findJust (\function -> getFunName function == functionName) functions in
    maybeZipWith bindZipper vns (map fst (getFunArgs function)) where
        bindZipper :: Maybe VariableName -> VariableName -> Maybe (VariableName, VariableName)
        bindZipper (Just a) b = Just (a,b)
        bindZipper Nothing _ = Nothing

-- | Generates simple 'DSInfo's without the info from function calls
generateDSI :: FunctionDeclaration -> [(VariableName, DSUse)] -> [DSInfo]
generateDSI fn dsus = let varGroups = groupBy (on (==) fst) dsus in
    map (\g -> DSI [(getFunName fn, fst.head $ g)] (map snd g)) varGroups

-- | Start the state monad to create a 'DSFun' for function
--generateDSF :: [FunctionName] -> FunctionDeclaration -> DSFun
--generateDSF = undefined
--generateDSF fnns fn = let (dsus, st) = runState (sumTerms step [getFunBody fn]) (AS fn fnns [] []) in
--    DSF fn (getStateCalls st) (generateDSI fn dsus)

-- | Function putting a variable definition in the context
putVar :: VariableName -> TermAnalyzer ()
putVar name = modify $ \s -> s {getStateVarNames = name:getStateVarNames s}

-- | Function returning 'True' if the variable is already defined
getVar :: VariableName -> TermAnalyzerState -> Bool
getVar name s = name `elem` getStateVarNames s


-- | Generate 'DSUse's for a single 'Term'
--step :: Term -> TermAnalyzer Output
--step = undefined
{-
step (VarInit name Ds) = do
    s <- get                        --TODO maybe gets?
    if getVar name s
        then error $ show name ++ " already initialized"
        else putVar name >> return []
                                     --TODO pointer copying

step (InitAssign name _ Ds) = do
    s <- get
    if getVar name s
        then error $ show name ++ " already initialized"
        else putVar name >> return []

step (While cond body) = stepBlock [cond,body,body]

step (Funcall name args) = do
    s <- get
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
            Var varname -> if getVar varname s
                then return [(varname, DSU op False False)]
                else error $ show varname ++ " not initialized before use in function " ++ show name
            _           -> error "Not implemented yet"

    return $ argDsus ++ funcallDsu-}

main = do
    ast <- parseMyFile "1.c"
    let (eithers, s) = runState (analyzeCTranslUnit ast) (AS Nothing [] [] [])
    printRecommendationFromAnalysis putStrLn (stupidMerge $ analyzeFunctions $ rights eithers)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

analyzeCTranslUnit :: CTranslUnit -> TermAnalyzer [Either Output DSFun] --TODO add global variables here
analyzeCTranslUnit (CTranslUnit extDecls _) = mapM analyzeCExtDecl extDecls

analyzeCExtDecl :: CExtDecl -> TermAnalyzer (Either Output DSFun) --TODO add global variables here
analyzeCExtDecl (CDeclExt decl)          = analyzeCDecl decl >>= return . Left
analyzeCExtDecl (CFDefExt cFunDef)       = analyzeCFunDef cFunDef >>= return . Right
analyzeCExtDecl a@(CAsmExt strLit dunno) = undefined  --HMMM do i want to play with asm

analyzeCDecl (CDecl specifiers tripleList _) = fmcs [analyzeCDeclSpecifiers specifiers, analyzeCTripleList tripleList]

manalyzeCInit :: Maybe CInit -> TermAnalyzer Output
manalyzeCInit = maybe (return []) analyzeCInit

analyzeCInit :: CInit -> TermAnalyzer Output
analyzeCInit (CInitExpr expr _) = analyzeCExpr expr
analyzeCInit (CInitList initList _) = analyzeCInitList initList

analyzeCInitList :: CInitList -> TermAnalyzer Output
analyzeCInitList initList = fmcs $ map (\(pds, init) -> fmcs $ analyzeCInit init : map analyzeCDesignator pds) initList

analyzeCTripleList :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> TermAnalyzer Output
analyzeCTripleList tripleList = fmcs $
    map manalyzeCDeclr       (map (\(f,_,_) -> f) tripleList) ++
    map manalyzeCInit        (map (\(_,s,_) -> s) tripleList) ++
    map manalyzeCExpr  (map (\(_,_,t) -> t) tripleList)

analyzeCDeclSpecifiers specifiers = return [] --TODO read doc on specifiers

analyzeCFunDef :: CFunDef -> TermAnalyzer DSFun
analyzeCFunDef (CFunDef specifiers declarator declarations statement _) = do
    body <- fmcs $
        [ analyzeCDeclSpecifiers specifiers
        , analyzeCDeclr declarator
        , analyzeCStatement statement] ++ map analyzeCDecl declarations
    funDec <- undefined
    modify $ \s -> undefined
    s <- get
    return $ DSF {getDSFFun = funDec, getDSFCalls = getStateCalls s, getDSFDSI = undefined }

analyzeCDerivedDeclarator :: CDerivedDeclr -> TermAnalyzer Output
analyzeCDerivedDeclarator (CPtrDeclr qualifs _) = fmcs $ map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CArrDeclr qualifs arrsize _) = fmcs $ analyzeCArraySize arrsize : map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CFunDeclr eidentpair attribs _) = undefined --FIXME implement this shit

analyzeCAttribute :: CAttr -> TermAnalyzer Output
analyzeCAttribute (CAttr ident exprs _) = fmcs $ map analyzeCExpr exprs

analyzeCTypeQualifier :: CTypeQual -> TermAnalyzer Output
analyzeCTypeQualifier (CConstQual _) = return []
analyzeCTypeQualifier (CVolatQual _) = return []
analyzeCTypeQualifier (CRestrQual _) = return []
analyzeCTypeQualifier (CInlineQual _) = return []
analyzeCTypeQualifier (CAttrQual attrib) = analyzeCAttribute attrib

analyzeCArraySize :: CArrSize -> TermAnalyzer Output
analyzeCArraySize (CNoArrSize _) = return []
analyzeCArraySize (CArrSize _ expr) = analyzeCExpr expr


manalyzeCDeclr :: Maybe CDeclr -> TermAnalyzer Output
manalyzeCDeclr = maybe (return []) analyzeCDeclr

analyzeCDeclr :: CDeclr -> TermAnalyzer Output
analyzeCDeclr (CDeclr mident derives mliteral attribs _) = fmcs $
    map analyzeCDerivedDeclarator derives ++ map analyzeCAttribute attribs

manalyzeCStatement :: Maybe CStat -> TermAnalyzer Output
manalyzeCStatement = maybe (return []) analyzeCStatement

analyzeCStatement :: CStat -> TermAnalyzer Output
analyzeCStatement (CLabel ident statement attribs _)        = fmcs $ analyzeCStatement statement : map analyzeCAttribute attribs
analyzeCStatement (CCase expr statement _)                  = fmcs [analyzeCExpr expr, analyzeCStatement statement]
analyzeCStatement (CCases expr1 expr2 statement _)          = fmcs $ analyzeCStatement statement : map analyzeCExpr [expr1, expr2]
analyzeCStatement (CDefault statement _)                    = analyzeCStatement statement
analyzeCStatement (CExpr mexpr _)                           = manalyzeCExpr mexpr
analyzeCStatement (CCompound idents compoundBlockItems _)   = fmcs $ map analyzeCCompoundBlockItem compoundBlockItems
analyzeCStatement (CIf expr statement mstatement _)         = fmcs
    [analyzeCExpr expr
    , analyzeCStatement statement
    , manalyzeCStatement mstatement] --TODO fix to substitute environments for cases
analyzeCStatement (CSwitch expr statement _)                = fmcs [analyzeCExpr expr, analyzeCStatement statement]
analyzeCStatement (CWhile expr statement isdowhile _)       = fmcs [analyzeCExpr expr, analyzeCStatement statement]
analyzeCStatement (CFor emexprdecl mexpr1 mexpr2 statement _)= fmcs $
    either manalyzeCExpr analyzeCDecl emexprdecl :
    analyzeCStatement statement :
    map manalyzeCExpr [mexpr1, mexpr2]
analyzeCStatement (CGoto ident _)                           = return [] --TODO implement goto
analyzeCStatement (CGotoPtr expr _)                         = analyzeCExpr expr --TODO implement goto
analyzeCStatement (CCont _)                                 = return []
analyzeCStatement (CBreak _)                                = return []
analyzeCStatement (CReturn mexpr _)                         = manalyzeCExpr mexpr
analyzeCStatement (CAsm asm _)                              = return [] --HMMM do i really want to care about somebody's asm?

analyzeCCompoundBlockItem :: CBlockItem -> TermAnalyzer Output
analyzeCCompoundBlockItem (CBlockStmt statement)            = analyzeCStatement statement
analyzeCCompoundBlockItem (CBlockDecl declaration)          = analyzeCDecl declaration
analyzeCCompoundBlockItem (CNestedFunDef funDef)            = return [] --TODO implement nested functions

analyzeCConst :: CConst -> TermAnalyzer Output
analyzeCConst (CIntConst int _)                             = return []
analyzeCConst (CCharConst char _)                           = return []
analyzeCConst (CFloatConst float _)                          = return []
analyzeCConst (CStrConst str _)                            = return []

manalyzeCExpr :: Maybe CExpr -> TermAnalyzer Output
manalyzeCExpr = maybe (return []) analyzeCExpr

isDsinfFunction :: FunctionName -> Bool --STUB
isDsinfFunction (F "insert") = True
isDsinfFunction (F "delete") = True
isDsinfFunction _ = False

-- | Puts a function call into the state
putCall :: FunctionName -> [CExpr] -> TermAnalyzer ()
putCall name exprs = do
    let cleanArgs = map justifyArgs exprs
    modify $ \s -> s {getStateCalls = (name, cleanArgs) : getStateCalls s} where
        justifyArgs :: CExpr -> Maybe VariableName
        justifyArgs (CVar (Ident v _ _) _) = Just (V v)
        justifyArgs _ = Nothing                      -- TODO function calls returning struct ds or a pointer, not only vars

analyzeCExpr :: CExpr -> TermAnalyzer Output
analyzeCExpr (CAlignofExpr expr _)                    = analyzeCExpr expr
analyzeCExpr (CAlignofType decln _)                   = analyzeCDecl decln
analyzeCExpr (CAssign assignop expr1 expr2 _)         = fmcs $ map analyzeCExpr [expr1, expr2]
analyzeCExpr (CBinary binop expr1 expr2 _)            = fmcs $ map analyzeCExpr [expr1, expr2]  
analyzeCExpr (CBuiltinExpr builtin)                   = analyzeCBuiltin builtin
analyzeCExpr (CCall (CVar (Ident funname _ _) _) exprs _) = do
    when (isDsinfFunction (F funname)) (putCall (F funname) exprs)
    fmcs $ map analyzeCExpr exprs
analyzeCExpr (CCall expr exprs _)                     = fmcs $ map analyzeCExpr (expr : exprs) --TODO calling a pointer
analyzeCExpr (CCast decln expr _)                     = fmcs $ [analyzeCDecl decln, analyzeCExpr expr]
analyzeCExpr (CComma exprs _)                         = fmcs $ map analyzeCExpr exprs
analyzeCExpr (CComplexImag expr _)                    = analyzeCExpr expr
analyzeCExpr (CComplexReal expr _)                    = analyzeCExpr expr
analyzeCExpr (CCompoundLit decln initList _)          = fmcs $ [analyzeCDecl decln, analyzeCInitList initList]
analyzeCExpr (CCond expr1 mexpr expr2 _)              = fmcs $ manalyzeCExpr mexpr : map analyzeCExpr [expr1, expr2]
analyzeCExpr (CConst const)                           = analyzeCConst const
analyzeCExpr (CIndex expr1 expr2 _)                   = fmcs $ map analyzeCExpr [expr1, expr2]
analyzeCExpr (CLabAddrExpr ident _)                   = return []
analyzeCExpr (CMember expr ident dereferred _)        = analyzeCExpr expr
analyzeCExpr (CSizeofExpr expr _)                     = analyzeCExpr expr
analyzeCExpr (CSizeofType decln _)                    = analyzeCDecl decln
analyzeCExpr (CStatExpr statement _)                  = analyzeCStatement statement
analyzeCExpr (CUnary unop expr _)                     = analyzeCExpr expr
analyzeCExpr (CVar ident _)                           = return []

analyzeCBuiltin :: CBuiltin -> TermAnalyzer Output
analyzeCBuiltin (CBuiltinVaArg expr decln _)                = fmcs [analyzeCExpr expr, analyzeCDecl decln]
analyzeCBuiltin (CBuiltinOffsetOf decl cPartDesns _)       = fmcs $ analyzeCDecl decl : map analyzeCDesignator cPartDesns
analyzeCBuiltin (CBuiltinTypesCompatible decln1 decln2 _)   = fmcs $ map analyzeCDecl [decln1, decln2]

analyzeCDesignator :: CDesignator -> TermAnalyzer Output
analyzeCDesignator (CArrDesig expr _)                   = analyzeCExpr expr
analyzeCDesignator (CMemberDesig ident _)               = return []
analyzeCDesignator (CRangeDesig expr1 expr2 _)          = fmcs $ map analyzeCExpr [expr1, expr2]

-- |Shortcut for 'fmap' 'concat' . 'sequence', useful in combining analysis of subterms
fmcs :: (Functor m, Monad m) => [m [a]] -> m [a]
fmcs = fmap concat . sequence

{-
Right (CTranslUnit
[ CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") (Just []) [] ) )] [] )
, CDeclExt (CDecl [CStorageSpec (CExtern ),CTypeSpec (CVoidType )] [(Just (CDeclr (Just "insert") [CFunDeclr (Right ([CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr Nothing [CPtrDeclr [] ] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] ,CDecl [CTypeSpec (CIntType )] [] ],False)) [] ] Nothing [] ),Nothing,Nothing)] )
, CFDefExt (CFunDef [CTypeSpec (CIntType )] (CDeclr (Just "main") [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType )] [(Just (CDeclr (Just "argc") [] Nothing [] ),Nothing,Nothing)] ,CDecl [CTypeQual (CConstQual ),CTypeSpec (CCharType )] [(Just (CDeclr (Just "argv") [CArrDeclr [] (CNoArrSize False) ,CPtrDeclr [] ] Nothing [] ),Nothing,Nothing)] ],False)) [] ] Nothing [] ) [] (CCompound []
	[CBlockDecl (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr (Just "d1") [] Nothing [] ),Nothing,Nothing)] )
	,CBlockStmt (CExpr (Just (CCall (CVar "insert" ) [CUnary CAdrOp (CVar "d1" ) ,CConst (CIntConst 4 )] )) )
	,CBlockStmt (CReturn (Just (CConst (CIntConst 0))))]))
])
-}
