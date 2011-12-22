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
    } deriving (Show, Eq)

-- | Data structure use case
data DSUse = DSU {
    getDSUName      :: OperationName,                           -- ^ Operation used
    isHeavilyUsed   :: Bool,                                    -- ^ Is it heavily used
    isUserDependent :: Bool                                     -- ^ Is it dependent on some external input (user, network, random, signals, etc.)
    } deriving (Show, Eq)

-- | Type for function definitions
data FunctionDeclaration = FunDecl {
    getFunName :: FunctionName,             -- ^ Name of the function
    getFunType :: Type,                     -- ^ Return type of the function
    getFunArgs :: [(VariableName, Type)]    -- ^ Names and types of the function arguments
    } deriving (Show, Eq)

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

-- | Term analyzer basic output, variables and 'DSUse's --TODO add global here?
type TermAnalyzerOutput = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateFunction :: Maybe FunctionDeclaration,              -- ^ Current function being analyzed
    getStateFunNames :: [FunctionName],                         -- ^ All the function names
    getStateVarNames :: [VariableName],                         -- ^ All the variable names --TODO are they only relevant ones?
    getStateCalls    :: [FunctionCall]                          -- ^ 'FunctionCall's gathered through the analysis
    } deriving (Show, Eq)

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

-- | Runs everything that is needed to analyze a program
analyze :: [FunctionDeclaration] -> [DSInfo]
analyze functions = let functionNames = map getFunName functions in
    let dsfs = map (generateDSF functionNames) functions in
    stupidMerge $ analyzeFunctions dsfs

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

-- | Like zipWith only returns only those elements of type 'c' that were qualified with Just
maybeZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
maybeZipWith f (x:xs) (y:ys) = case f x y of
    Just z -> z : maybeZipWith f xs ys
    Nothing -> maybeZipWith f xs ys
maybeZipWith _ _ _ = []

-- | Generates simple 'DSInfo's without the info from function calls
generateDSI :: FunctionDeclaration -> [(VariableName, DSUse)] -> [DSInfo]
generateDSI fn dsus = let varGroups = groupBy (on (==) fst) dsus in
    map (\g -> DSI [(getFunName fn, fst.head $ g)] (map snd g)) varGroups

-- | Start the state monad to create a 'DSFun' for function
generateDSF :: [FunctionName] -> FunctionDeclaration -> DSFun
generateDSF = undefined
--generateDSF fnns fn = let (dsus, st) = runState (sumTerms step [getFunBody fn]) (AS fn fnns [] []) in
--    DSF fn (getStateCalls st) (generateDSI fn dsus)

-- | Function putting a variable definition in the context
putVar :: VariableName -> TermAnalyzer ()
putVar name = modify $ \s -> s {getStateVarNames = name:getStateVarNames s}

-- | Function returning 'True' if the variable is already defined
getVar :: VariableName -> TermAnalyzerState -> Bool
getVar name s = name `elem` getStateVarNames s


-- | Generate 'DSUse's for a single 'Term'
step :: Term -> TermAnalyzer TermAnalyzerOutput
step = undefined
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
    let dsfs = runState (analyzeProgram ast) (AS Nothing [] [] [])
    printRecommendationFromAnalysis putStrLn (stupidMerge $ analyzeFunctions dsfs)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

analyzeProgram :: CTranslUnit -> TermAnalyzer [DSFun] --TODO add global variables here
analyzeProgram (CTranslUnit extDecls _) = mapM analyzeExtDecl extDecls

analyzeExtDecl :: CExtDecl -> TermAnalyzer DSFun --TODO add global variables here
analyzeExtDecl (CDeclExt cdecl)         = return [] -- STUB
analyzeExtDecl (CFDefExt cfundef)       = analyzeCFunDef cfundef
--analyzeExtDecl a@(CAsmExt strLit dunno) = return  --HMMM do i want to play with asm

analyzeCDecl (CDecl specifiers declList _) = fmcs [analyzeCDeclSpecifiers specifiers, analyzeCDeclDeclList declList]

analyzeCDeclSpecifiers specifiers = return [] --TODO read doc on specifiers

analyzeCDeclDeclList :: [CDeclr] -> TermAnalyzer TermAnalyzerOutput
analyzeCDeclDeclList = fmcs . map analyzeCDeclarator

analyzeCFunDef :: CFunDef -> TermAnalyzer DSFun
analyzeCFunDef (CFunDef specifiers declarator declarations statement _) = do
    body <- fmcs $
        [ analyzeCDeclSpecifiers specifiers
        , analyzeCDeclarator declarator
        , analyzeCStatement statement] ++ map analyzeCDecl declarations
    modify $ \s -> undefined

analyzeCDerivedDeclarator :: CDerivedDeclr -> TermAnalyzer TermAnalyzerOutput
analyzeCDerivedDeclarator (CPtrDeclr qualifs _) = map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CArrDeclr qualifs arrsize _) = fmcs $ analyzeCArraySize arrsize : map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CFunDeclr eidentpair attribs _) = undefined --FIXME implement this shit

analyzeCAttribute :: CAttr -> TermAnalyzer TermAnalyzerOutput
analyzeCAttribute (CAttr ident exprs _) = map analyzeCExpression exprs

analyzeCTypeQualifier :: CTypeQual -> TermAnalyzer TermAnalyzerOutput
analyzeCTypeQualifier (CConstQual _) = return []
analyzeCTypeQualifier (CVolatQual _) = return []
analyzeCTypeQualifier (CRestrQual _) = return []
analyzeCTypeQualifier (CInlineQual _) = return []
analyzeCTypeQualifier (CAttrQual attrib) = analyzeCAttribute attrib

analyzeCArraySize :: CArrSize -> TermAnalyzer TermAnalyzerOutput
analyzeCArraySize (CNoArrSize _) = return []
analyzeCArraySize (CArrSize _ expr) = analyzeCExpression expr

analyzeCDeclarator :: CDeclr -> TermAnalyzer TermAnalyzerOutput
analyzeCDeclarator (CDeclr mident derives mliteral attribs _) = fmcs $
    map analyzeCDerivedDeclarator derives ++ map analyzeCAttribute attribs

manalyzeCStatement :: Maybe CStat -> TermAnalyzer TermAnalyzerOutput
manalyzeCStatement = maybe (return []) analyzeCStatement

analyzeCStatement :: CStat -> TermAnalyzer TermAnalyzerOutput
analyzeCStatement (CLabel ident statement attrib _)         = analyzeCStatement statement
analyzeCStatement (CCase expr statement _)                  = fmcs [analyzeCExpression expr, analyzeCStatement statement]
analyzeCStatement (CCases expr1 expr2 statement _)          = fmcs $ analyzeCStatement statement : map analyzeCExpression [expr1, expr2]
analyzeCStatement (CDefault statement _)                    = analyzeCStatement statement
analyzeCStatement (CExpr mexpr _)                           = manalyzeCExpression mexpr
analyzeCStatement (CCompound idents compoundBlockItems _)   = fmcs $ map analyzeCCompoundBlockItem compoundBlockItems
analyzeCStatement (CIf expr statement mstatement _)         = fmcs
    [analyzeCExpression expr
    , analyzeCStatement statement
    , manalyzeCStatement mstatement] --TODO fix to substitute environments for cases
analyzeCStatement (CSwitch expr statement _)                = fmcs [analyzeCExpression expr, analyzeCStatement statement]
analyzeCStatement (CWhile expr statement isdowhile _)       = fmcs [analyzeCExpression expr, analyzeCStatement statement]
analyzeCStatement (CFor emexprdecl mexpr1 mexpr2 statement _)= fmcs $
    either manalyzeCExpression analyzeCDecl emexprdecl :
    analyzeCStatement statement :
    map manalyzeCExpression [mexpr1, mexpr2]
analyzeCStatement (CGoto ident _)                           = return [] --TODO implement goto
analyzeCStatement (CGotoPtr expr _)                         = analyzeCExpression expr --TODO implement goto
analyzeCStatement (CCont _)                                 = return []
analyzeCStatement (CBreak _)                                = return []
analyzeCStatement (CReturn mexpr _)                         = manalyzeCExpression mexpr
analyzeCStatement (CAsm asm _)                              = return [] --HMMM do i really want to care about somebody's asm?

analyzeCCompoundBlockItem :: CBlockItem -> TermAnalyzer TermAnalyzerOutput
analyzeCCompoundBlockItem (CBlockStmt statement)            = analyzeCStatement statement
analyzeCCompoundBlockItem (CBlockDecl declaration)          = analyzeCDecl declaration
analyzeCCompoundBlockItem (CNestedFunDef funDef)            = return [] --TODO implement nested functions

analyzeCConst :: CConst -> TermAnalyzer TermAnalyzerOutput
analyzeCConst (CIntConst int _)                             = return []

manalyzeCExpression :: Maybe CExpr -> TermAnalyzer TermAnalyzerOutput
manalyzeCExpression = maybe (return []) analyzeCExpression

-- | Puts a function call into the state
putCall :: FunctionName -> [CExpr] -> TermAnalyzer ()
putCall name exprs = do
    let cleanArgs = map justifyArgs exprs
    modify $ \s -> s {getStateCalls = (name, cleanArgs) : getStateCalls s} where
        justifyArgs :: CExpr -> Maybe VariableName
        justifyArgs (CVar (Ident v _ _)) = Just v
        justifyArgs _ = Nothing                      -- TODO function calls returning struct ds or a pointer, not only vars

analyzeCExpression :: CExpr -> TermAnalyzer TermAnalyzerOutput
analyzeCExpression (CAlignofExpr expr _)                    = analyzeCExpression expr
analyzeCExpression (CAlignofType decln _)                   = analyzeCDecl decln
analyzeCExpression (CAssign assignop expr1 expr2 _)         = fmcs $ map analyzeCExpression [expr1, expr2]
analyzeCExpression (CBinary binop expr1 expr2 _)            = fmcs $ map analyzeCExpression [expr1, expr2]  
analyzeCExpression (CBuiltinExpr builtin)                   = analyzeCBuiltin builtin
analyzeCExpression (CCall expr@(CVar (Ident funname _ _)exprs _)) = do
    if isDsinfFunction funname
        then putCall funname exprs
        else return ()
    fmcs $ map analyzeCExpression (expr : exprs)
analyzeCExpression (CCall expr exprs _)                     = fmcs $ map analyzeCExpression (expr : exprs) --TODO calling a pointer
analyzeCExpression (CCast decln expr _)                     = fmcs $ [analyzeCDecl decln, analyzeCExpression expr]
analyzeCExpression (CComma exprs _)                         = fmcs $ map analyzeCExpression exprs
analyzeCExpression (CComplexImag expr _)                    = analyzeCExpression expr
analyzeCExpression (CComplexReal expr _)                    = analyzeCExpression expr
analyzeCExpression (CCompoundLit decln initList _)          = fmcs $ [analyzeCDecl decln, analyzeInitializerList initList]
analyzeCExpression (CCond expr1 mexpr expr2 _)              = fmcs $ manalyzeCExpression mexpr : map analyzeCExpression [expr1, expr2]
analyzeCExpression (CConst const)                           = analyzeCConst const
analyzeCExpression (CIndex expr1 expr2 _)                   = fmcs $ map analyzeCExpression [expr1, expr2]
analyzeCExpression (CLabAddrExpr ident _)                   = return []
analyzeCExpression (CMember expr ident dereferred _)        = analyzeCExpression expr
analyzeCExpression (CSizeofExpr expr _)                     = analyzeCExpression expr
analyzeCExpression (CSizeofType decln _)                    = analyzeCDecl decln
analyzeCExpression (CStatExpr statement _)                  = analyzeCStatement statement
analyzeCExpression (CUnary unop expr _)                     = analyzeCExpression expr
analyzeCExpression (CVar ident _)                           = return []

analyzeInitializerList :: CInitList -> TermAnalyzer TermAnalyzerOutput
analyzeInitializerList cInitList = fmcs $
    map
        (\(partDesigns, initializer) -> fmcs $
             analyzeInitializer initializer :
             map analyzeCPartDesignator partDesigns)
        cInitList

analyzeInitializer :: CInit -> TermAnalyzer TermAnalyzerOutput
analyzeInitializer (CInitExpr expr _)                       = analyzeCExpression expr
analyzeInitializer (CInitList initList _)                   = analyzeInitializerList initList

analyzeCBuiltin :: CBuiltin -> TermAnalyzer TermAnalyzerOutput
analyzeCBuiltin (CBuiltinVaArg expr decln _)                = fmcs [analyzeCExpression expr, analyzeCDecl decln]
analyzeCBuiltin (CBuiltinOffsetOf decln cPartDesns _)       = fmcs $ map analyzeCPartDesignator cPartDesns
analyzeCBuiltin (CBuiltinTypesCompatible decln1 decln2 _)   = fmcs $ map analyzeCDecl [decln1, decln2]

analyzeCPartDesignator :: CDesignator -> TermAnalyzer TermAnalyzerOutput
analyzeCPartDesignator (CArrDesig expr _)                   = analyzeCExpression expr
analyzeCPartDesignator (CMemberDesig ident _)               = return []
analyzeCPartDesignator (CRangeDesig expr1 expr2 _)          = fmcs $ map analyzeCExpression [expr1, expr2]

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
