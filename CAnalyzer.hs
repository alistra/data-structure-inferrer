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

import Debug.Trace


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
    getFunName :: FunctionName,                                 -- ^ Name of the function
    getFunType :: CTypeSpec,                                    -- ^ Return type of the function
    getFunArgs :: [(VariableName, CTypeSpec)]                   -- ^ Names and types of the function arguments
    } deriving (Show)

-- | State monad with 'TermAnalyzerState'
type TermAnalyzer a = State TermAnalyzerState a

-- | Term analyzer basic output, variables and 'DSUse's
type Output = [(VariableName, DSUse)]

-- | State of the analyzer
data TermAnalyzerState = AS {
    getStateFunction :: Maybe FunctionDeclaration,              -- ^ Current function being analyzed
    getStateCalls    :: [FunctionCall]                          -- ^ 'FunctionCall's gathered through the analysis
    } deriving (Show)

append :: TermAnalyzerState -> TermAnalyzerState -> TermAnalyzerState
append (AS f1 cs1) (AS _ cs2) = AS f1 (cs1 `union` cs2)

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
generateDSI :: FunctionName -> Output -> [DSInfo]
generateDSI funName dsus = let varGroups = groupBy (on (==) fst) dsus in
    map (\g -> DSI [(funName, fst.head $ g)] (map snd g)) varGroups

-- | Start the state monad to create a 'DSFun' for function
--generateDSF :: [FunctionName] -> FunctionDeclaration -> DSFun
--generateDSF = undefined
--generateDSF fnns fn = let (dsus, st) = runState (sumTerms step [getFunBody fn]) (AS fn fnns [] []) in
--    DSF fn (getStateCalls st) (generateDSI fn dsus)

{-
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
    let (eithers, s) = runState (analyzeCTranslUnit ast) (AS Nothing [])
    printRecommendationFromAnalysis putStrLn (stupidMerge $ analyzeFunctions $ rights eithers)

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

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

getName :: CDeclr -> String
getName (CDeclr (Just (Ident str _ _)) _ _ _ _) = str
getName (CDeclr Nothing _ _ _ _) = error "function without a name? that's just ridiculous"

getType :: [CDeclSpec] -> CTypeSpec
getType declSpecs = let (_,_,_,specs,_) = partitionDeclSpecs declSpecs in
    if length specs > 1
        then error $ show specs -- >:D
        else head specs

getArgsWithTypes :: CDeclr -> [(VariableName, CTypeSpec)]
getArgsWithTypes declr = [] --STUB

analyzeCTranslUnit :: CTranslUnit -> TermAnalyzer [Either Output DSFun] --TODO add global variables here
analyzeCTranslUnit (CTranslUnit extDecls _) = mapM analyzeCExtDecl extDecls

analyzeCExtDecl :: CExtDecl -> TermAnalyzer (Either Output DSFun) --TODO add global variables here
analyzeCExtDecl (CDeclExt decl)          = analyzeCDecl decl >>= return . Left
analyzeCExtDecl (CFDefExt cFunDef)       = analyzeCFunDef cFunDef >>= return . Right
analyzeCExtDecl a@(CAsmExt strLit dunno) = return $ Left [] --HMMM do i want to play with asm

analyzeCDecl (CDecl declSpecs tripleList _) = fmcs [analyzeCDeclSpecs declSpecs, analyzeCTripleList tripleList]

analyzeCInit :: CInit -> TermAnalyzer Output
analyzeCInit (CInitExpr expr _) = analyzeCExpr expr
analyzeCInit (CInitList initList _) = analyzeCInitList initList

analyzeCInitList :: CInitList -> TermAnalyzer Output
analyzeCInitList initList = fmcs $ map (\(pds, init) -> fmcs $ analyzeCInit init : map analyzeCDesignator pds) initList

analyzeCTripleList :: [(Maybe CDeclr, Maybe CInit, Maybe CExpr)] -> TermAnalyzer Output
analyzeCTripleList tripleList = fmcs $
    map manalyzeCDeclr       (map (\(f,_,_) -> f) tripleList) ++
    map manalyzeCInit        (map (\(_,s,_) -> s) tripleList) ++
    map manalyzeCExpr        (map (\(_,_,t) -> t) tripleList)

analyzeCDeclSpecs declSpecs = let (_,attribs,_,_,_) = partitionDeclSpecs declSpecs in
    fmcs $ map analyzeCAttr attribs

analyzeCFunDef :: CFunDef -> TermAnalyzer DSFun
analyzeCFunDef (CFunDef declSpecs declr declarations statement _) = do
    let funDec = FunDecl (F $ getName declr) (getType declSpecs) (getArgsWithTypes declr)
    modify $ \s -> s {getStateFunction = Just $ funDec, getStateCalls = []}
    body <- fmcs $
        [ analyzeCDeclSpecs declSpecs
        , analyzeCDeclr declr
        , analyzeCStat statement] ++ map analyzeCDecl declarations
    s <- get
    modify $ \s -> s {getStateFunction = Nothing}
    return $ DSF {getDSFFun = funDec, getDSFCalls = getStateCalls s, getDSFDSI = generateDSI (getFunName funDec) (traceShow body body)}

analyzeCDerivedDeclarator :: CDerivedDeclr -> TermAnalyzer Output
analyzeCDerivedDeclarator (CPtrDeclr qualifs _) = fmcs $ map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CArrDeclr qualifs arrsize _) = fmcs $ analyzeCArraySize arrsize : map analyzeCTypeQualifier qualifs
analyzeCDerivedDeclarator (CFunDeclr eidentpair attribs _) = return [] --undefined --FIXME implement this shit

analyzeCAttr :: CAttr -> TermAnalyzer Output
analyzeCAttr (CAttr ident exprs _) = fmcs $ map analyzeCExpr exprs

analyzeCTypeQualifier :: CTypeQual -> TermAnalyzer Output
analyzeCTypeQualifier (CConstQual _) = return []
analyzeCTypeQualifier (CVolatQual _) = return []
analyzeCTypeQualifier (CRestrQual _) = return []
analyzeCTypeQualifier (CInlineQual _) = return []
analyzeCTypeQualifier (CAttrQual attrib) = analyzeCAttr attrib

analyzeCArraySize :: CArrSize -> TermAnalyzer Output
analyzeCArraySize (CNoArrSize _) = return []
analyzeCArraySize (CArrSize _ expr) = analyzeCExpr expr

analyzeCDeclr :: CDeclr -> TermAnalyzer Output
analyzeCDeclr (CDeclr mident derives mliteral attribs _) = fmcs $
    map analyzeCDerivedDeclarator derives ++ map analyzeCAttr attribs

analyzeCStat :: CStat -> TermAnalyzer Output
analyzeCStat (CLabel ident statement attribs _)        = fmcs $ analyzeCStat statement : map analyzeCAttr attribs
analyzeCStat (CCase expr statement _)                  = fmcs [analyzeCExpr expr, analyzeCStat statement]
analyzeCStat (CCases expr1 expr2 statement _)          = fmcs $ analyzeCStat statement : map analyzeCExpr [expr1, expr2]
analyzeCStat (CDefault statement _)                    = analyzeCStat statement
analyzeCStat (CExpr mexpr _)                           = manalyzeCExpr mexpr
analyzeCStat (CCompound idents compoundBlockItems _)   = fmcs $ map analyzeCCompoundBlockItem compoundBlockItems
analyzeCStat (CIf expr statement mstatement _)         = fmcs
    [analyzeCExpr expr
    , analyzeCStat statement
    , manalyzeCStat mstatement] --TODO fix to substitute environments for cases
analyzeCStat (CSwitch expr statement _)                = fmcs [analyzeCExpr expr, analyzeCStat statement]
analyzeCStat (CWhile expr statement isdowhile _)       = fmcs [analyzeCExpr expr, analyzeCStat statement]
analyzeCStat (CFor emexprdecl mexpr1 mexpr2 statement _)= fmcs $
    either manalyzeCExpr analyzeCDecl emexprdecl :
    analyzeCStat statement :
    map manalyzeCExpr [mexpr1, mexpr2]
analyzeCStat (CGoto ident _)                           = return [] --TODO implement goto
analyzeCStat (CGotoPtr expr _)                         = analyzeCExpr expr --TODO implement goto
analyzeCStat (CCont _)                                 = return []
analyzeCStat (CBreak _)                                = return []
analyzeCStat (CReturn mexpr _)                         = manalyzeCExpr mexpr
analyzeCStat (CAsm asm _)                              = return [] --HMMM do i really want to care about somebody's asm?

analyzeCCompoundBlockItem :: CBlockItem -> TermAnalyzer Output
analyzeCCompoundBlockItem (CBlockStmt statement)            = analyzeCStat statement
analyzeCCompoundBlockItem (CBlockDecl declaration)          = analyzeCDecl declaration
analyzeCCompoundBlockItem (CNestedFunDef funDef)            = return [] --TODO implement nested functions

analyzeCExpr :: CExpr -> TermAnalyzer Output
analyzeCExpr (CAlignofExpr expr _)                    = analyzeCExpr expr
analyzeCExpr (CAlignofType decln _)                   = analyzeCDecl decln
analyzeCExpr (CAssign assignop expr1 expr2 _)         = fmcs $ map analyzeCExpr [expr1, expr2]
analyzeCExpr (CBinary binop expr1 expr2 _)            = fmcs $ map analyzeCExpr [expr1, expr2]  
analyzeCExpr (CBuiltinExpr builtin)                   = analyzeCBuiltin builtin
analyzeCExpr (CCall (CVar (Ident funName _ _) _) ((CVar (Ident varName _ _) _):exprs) _) = do
    when (isDsinfFunction (F funName)) (putCall (F funName) exprs) --TODO use nicer name matching and checking
    analysis <- fmcs $ map analyzeCExpr exprs
    let mdsu = case funName of
            "insert"        -> Just InsertVal
            "find"          -> Just FindByVal
            "update"        -> Just UpdateByRef
            "delete"        -> Just DeleteByRef
            "max"           -> Just ExtremalVal
            "delete_max"    -> Just DeleteExtremalVal
            _               -> Nothing
    maybe (return analysis) (\dsu -> return $ (V varName,DSU dsu False False) : analysis) mdsu --TODO get varname of the ds
analyzeCExpr (CCall expr exprs _)                     = fmcs $ map analyzeCExpr (expr : exprs) --TODO calling a pointer
analyzeCExpr (CCast decln expr _)                     = fmcs $ [analyzeCDecl decln, analyzeCExpr expr]
analyzeCExpr (CComma exprs _)                         = fmcs $ map analyzeCExpr exprs
analyzeCExpr (CComplexImag expr _)                    = analyzeCExpr expr
analyzeCExpr (CComplexReal expr _)                    = analyzeCExpr expr
analyzeCExpr (CCompoundLit decln initList _)          = fmcs $ [analyzeCDecl decln, analyzeCInitList initList]
analyzeCExpr (CCond expr1 mexpr expr2 _)              = fmcs $ manalyzeCExpr mexpr : map analyzeCExpr [expr1, expr2]
analyzeCExpr (CConst const)                           = return []
analyzeCExpr (CIndex expr1 expr2 _)                   = fmcs $ map analyzeCExpr [expr1, expr2]
analyzeCExpr (CLabAddrExpr ident _)                   = return []
analyzeCExpr (CMember expr ident dereferred _)        = analyzeCExpr expr
analyzeCExpr (CSizeofExpr expr _)                     = analyzeCExpr expr
analyzeCExpr (CSizeofType decln _)                    = analyzeCDecl decln
analyzeCExpr (CStatExpr statement _)                  = analyzeCStat statement
analyzeCExpr (CUnary unop expr _)                     = analyzeCExpr expr
analyzeCExpr (CVar ident _)                           = return []

analyzeCBuiltin :: CBuiltin -> TermAnalyzer Output
analyzeCBuiltin (CBuiltinVaArg expr decl _)             = fmcs [analyzeCExpr expr, analyzeCDecl decl]
analyzeCBuiltin (CBuiltinOffsetOf decl cPartDesns _)    = fmcs $ analyzeCDecl decl : map analyzeCDesignator cPartDesns
analyzeCBuiltin (CBuiltinTypesCompatible decl1 decl2 _) = fmcs $ map analyzeCDecl [decl1, decl2]

analyzeCDesignator :: CDesignator -> TermAnalyzer Output
analyzeCDesignator (CArrDesig expr _)                   = analyzeCExpr expr
analyzeCDesignator (CMemberDesig ident _)               = return []
analyzeCDesignator (CRangeDesig expr1 expr2 _)          = fmcs $ map analyzeCExpr [expr1, expr2]

-- |Shortcut for 'fmap' 'concat' . 'sequence', useful in combining analysis of subterms
fmcs :: (Functor m, Monad m) => [m [a]] -> m [a]
fmcs = fmap concat . sequence

-- | Wrapper for analyzing 'Maybe' values
ma :: (a -> TermAnalyzer Output) -> (Maybe a -> TermAnalyzer Output)
ma = maybe (return [])

manalyzeCDeclr :: Maybe CDeclr -> TermAnalyzer Output
manalyzeCDeclr = ma analyzeCDeclr

manalyzeCStat :: Maybe CStat -> TermAnalyzer Output
manalyzeCStat = ma analyzeCStat

manalyzeCExpr :: Maybe CExpr -> TermAnalyzer Output
manalyzeCExpr = ma analyzeCExpr

manalyzeCInit :: Maybe CInit -> TermAnalyzer Output
manalyzeCInit = ma analyzeCInit

