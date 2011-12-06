module CAnalyzer where

import Language.C
import Language.C.System.GCC
import Data.Maybe

main = parseMyFile "1.c" >>= analyzeProgram

-- TODO this is the type of something gathering dsuse
-- fmap concat . sequence

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

--analyzeProgram = return . prettyUsingInclude
analyzeProgram (CTranslUnit extDecls _) = putStrLn "Analyzing Translation Unit" >> mapM_ analyzeExtDecl extDecls

analyzeExtDecl (CDeclExt cdecl) = putStrLn "Analyzing CDeclExt" >> analyzeCDecl cdecl
analyzeExtDecl (CFDefExt cfundef) = putStrLn "Analyzing CFDefExt" >> analyzeCFunDef cfundef
analyzeExtDecl a@(CAsmExt strLit dunno) = putStrLn "Analyzing CAsmExt" >> print a

analyzeCDecl (CDecl specifiers declList _) = do
    putStrLn "Analyzing CDecl"
    analyzeCDeclSpecifiers specifiers
    analyzeCDeclDeclList declList

analyzeCDeclSpecifiers specifiers = print "specifiers"

analyzeCDeclDeclList  = mapM_ analyzeCDeclarator

analyzeCFunDef (CFunDef specifiers declarator declaration statement _) = do
    putStrLn "Analyzing CFunDef"
    analyzeCDeclSpecifiers specifiers
    analyzeCDeclarator declarator
    analyzeCDeclaration declaration
    analyzeCStatement statement

analyzeCDeclarator declarator = putStrLn "Analyzing CDeclarator" >> print declarator

analyzeCDeclaration declaration = putStrLn "Analyzing CDeclaration" >> print declaration

analyzeCStatement (CLabel ident statement attrib _) = do
    putStrLn "Analyzing CLabel"
    print ident
    analyzeCStatement statement
    print attrib

analyzeCStatement (CCase expr statement _)                  = do
    putStrLn "Analyzing CCase"
    analyzeCExpression expr
    analyzeCStatement statement

analyzeCStatement (CCases expr1 expr2 statement _)          = do
    putStrLn "Analyzing CStatement"
    mapM_ analyzeCExpression [expr1, expr2]
    analyzeCStatement statement

analyzeCStatement (CDefault statement _)                    = putStrLn "Analyzing CDefault" >> analyzeCStatement statement

analyzeCStatement (CExpr mexpr _)                           = putStrLn "Analyzing CExpr" >> analyzeCExpression (fromJust mexpr)

analyzeCStatement (CCompound idents compoundBlockItems _) = do
    putStrLn "Analyzing CCompound"
    print idents
    mapM_ analyzeCCompoundBlockItem compoundBlockItems

analyzeCStatement (CIf expr statement mstatement _)         = do
    putStrLn "Analyzing CIf"
    analyzeCExpression expr
    analyzeCStatement statement
    analyzeCStatement (fromJust mstatement)

analyzeCStatement (CSwitch expr statement _)                = do
    putStrLn "Analyzing CSwitch"
    analyzeCExpression expr
    analyzeCStatement statement

analyzeCStatement (CWhile expr statement bool _)            = do
    putStrLn "Analyzing CWhile"
    analyzeCExpression expr
    analyzeCStatement statement
    print bool

analyzeCStatement (CFor either mexpr1 mexpr2 statement _)   = putStrLn "Analyzing CStatement" >> print "for" --TODO read doc
analyzeCStatement (CGoto ident _)                           = putStrLn "Analyzing CGoto"
analyzeCStatement (CGotoPtr expr _)                         = putStrLn "Analyzing CGotoPtr" >> analyzeCExpression expr
analyzeCStatement (CCont _)                                 = putStrLn "Analyzing CCont"
analyzeCStatement (CBreak _)                                = putStrLn "Analyzing CBreak"
analyzeCStatement (CReturn mexpr _)                         = putStrLn "Analyzing CReturn" >> analyzeCExpression (fromJust mexpr)
analyzeCStatement (CAsm asm _)                              = putStrLn "Analyzing CAsm"

analyzeCCompoundBlockItem (CBlockStmt statement)            = putStrLn "Analyzing CBlockStmt" >> analyzeCStatement statement
analyzeCCompoundBlockItem (CBlockDecl declaration)          = putStrLn "Analyzing CBlockDecl" >> analyzeCDecl declaration
analyzeCCompoundBlockItem (CNestedFunDef funDef)            = putStrLn "Analyzing CNestedFunDef" >> print funDef

analyzeCConst (CIntConst int _)                             = putStrLn "Analyzing CIntConst" >> print int

analyzeCExpression (CComma exprs _)                         = putStrLn "Analyzing CComma" >> mapM_ analyzeCExpression exprs
analyzeCExpression (CAssign assignop expr1 expr2 _)         = putStrLn "Analyzing CAssign" >> print assignop >> mapM_ analyzeCExpression [expr1, expr2]
analyzeCExpression (CCond expr1 mexpr expr2 _)              = putStrLn "Analyzing CCond" >> analyzeCExpression (fromJust mexpr) >> mapM_ analyzeCExpression [expr1, expr2]
analyzeCExpression (CBinary binop expr1 expr2 _)            = putStrLn "Analyzing CBinary" >> print binop >> mapM_ analyzeCExpression [expr1, expr2]  
analyzeCExpression (CCall expr exprs _)                     = putStrLn "Analyzing CCall" >> mapM_ analyzeCExpression (expr : exprs)
analyzeCExpression (CVar ident _)                           = putStrLn "Analyzing CVar" >> print ident
analyzeCExpression (CUnary unop expr _)                     = putStrLn "Analyzing CUnary" >> print unop >> analyzeCExpression expr
analyzeCExpression (CConst const)                           = putStrLn "Analyzing CConst" >> analyzeCConst const
analyzeCExpression (CSizeofExpr expr _)                     = putStrLn "Analyzing CSizeofExpr" >> analyzeCExpression expr
analyzeCExpression (CSizeofType declaration _)              = putStrLn "Analyzing CSizeofType"
analyzeCExpression (CIndex expr1 expr2 _)                   = putStrLn "Analyzing CIndex" >> mapM_ analyzeCExpression [expr1, expr2]
analyzeCExpression (CComplexReal expr _)                    = putStrLn "Analyzing CComplexReal" >> analyzeCExpression expr
analyzeCExpression (CComplexImag expr _)                    = putStrLn "Analyzing CComplexImag" >> analyzeCExpression expr
analyzeCExpression (CStatExpr statement _)                  = putStrLn "Analyzing CStatExpr" >> analyzeCStatement statement
analyzeCExpression (CCast declaration expr _)               = putStrLn "Analyzing CCast" >> analyzeCDeclaration >> analyzeCExpression expr     

analyzeCExpression a = error . show $ a
{-
analyzeCExpression CAlignofExpr (CExpression a) a   
analyzeCExpression CAlignofType (CDeclaration a) a  
analyzeCExpression CMember (CExpression a) Ident Bool a     
analyzeCExpression CCompoundLit (CDeclaration a) (CInitializerList a) a    
analyzeCExpression CLabAddrExpr Ident a
analyzeCExpression CBuiltinExpr (CBuiltinThing a)
-}
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
