module CAnalyzer where

import Language.C
import Language.C.System.GCC

main = parseMyFile "1.c" >>=  analyzeProgram

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

--analyzeProgram = prettyUsingInclude
analyzeProgram a@(CTranslUnit extDecls _) = putStrLn "Analyzing Translation Unit" >> mapM_ analyzeExtDecl extDecls

analyzeExtDecl a@(CDeclExt cdecl) = putStrLn "Analyzing CDeclExt" >> analyzeCDecl cdecl
analyzeExtDecl a@(CFDefExt cfundef) = putStrLn "Analyzing CFDefExt" >> analyzeCFunDef cfundef
analyzeExtDecl a@(CAsmExt strLit dunno) = putStrLn "Analyzing CAsmExt" >> print a

analyzeCDecl a@(CDecl specifiers declList _) = do
    putStrLn "Analyzing CDecl"
    analyzeCDeclSpecifiers specifiers
    analyzeCDeclDeclList declList

analyzeCDeclSpecifiers specifiers = print "specifiers"

analyzeCDeclDeclList declList = print "decl list"

analyzeCFunDef (CFunDef specifiers declarator declaration statement _) = do
    putStrLn "Analyzing CFunDef"
    analyzeCDeclSpecifiers specifiers
    analyzeCDeclarator declarator
    analyzeCDeclaration declaration
    analyzeCStatement statement

analyzeCDeclarator declarator = putStrLn "Analyzing CDeclarator" >> print declarator

analyzeCDeclaration declaration = putStrLn "Analyzing CDeclaration" >> print declaration

analyzeCStatement (CLabel ident statement attrib _) = do
    putStrLn "Analyzing CStatement"
    print "label"
    analyzeCStatement statement

analyzeCStatement (CCase expr statement _) = putStrLn "Analyzing CStatement" >> print "case" >> analyzeCStatement statement
analyzeCStatement (CCases expr1 expr2 statement _)          = putStrLn "Analyzing CStatement" >> print "cases"
analyzeCStatement (CDefault statement _)                    = putStrLn "Analyzing CStatement" >> print "default"
analyzeCStatement (CExpr mexpr _)                           = putStrLn "Analyzing CStatement" >> print "expr"
analyzeCStatement (CCompound idents compoundBlockItems _) = do
    putStrLn "Analyzing CStatement"
    print idents
    mapM_ analyzeCCompoundBlockItem compoundBlockItems

analyzeCStatement (CIf expr statement mstatement _)         = putStrLn "Analyzing CStatement" >> print "if"
analyzeCStatement (CSwitch expr statement _)                = putStrLn "Analyzing CStatement" >> print "switch"
analyzeCStatement (CWhile expr statement bool _)            = putStrLn "Analyzing CStatement" >> print "while"
analyzeCStatement (CFor either mexpr1 mexpr2 statement _)   = putStrLn "Analyzing CStatement" >> print "for"
analyzeCStatement (CGoto ident _)                           = putStrLn "Analyzing CStatement" >> print "goto"
analyzeCStatement (CGotoPtr expr _)                         = putStrLn "Analyzing CStatement" >> print "gotoPtr"
analyzeCStatement (CCont _)                                 = putStrLn "Analyzing CStatement" >> print "continue"
analyzeCStatement (CBreak _)                                = putStrLn "Analyzing CStatement" >> print "break"
analyzeCStatement (CReturn mexpr _)                         = putStrLn "Analyzing CStatement" >> print "return"
analyzeCStatement (CAsm asm _)                              = putStrLn "Analyzing CStatement" >> print "asm"

analyzeCCompoundBlockItem (CBlockStmt statement)            = putStrLn "Analyzing CBlockStmt" >> analyzeCStatement statement
analyzeCCompoundBlockItem (CBlockDecl declaration)          = putStrLn "Analyzing CBlockDecl" >> analyzeCDecl declaration
analyzeCCompoundBlockItem (CNestedFunDef funDef)            = putStrLn "Analyzing CNestedFunDef" >> print funDef

{-analyzeCExpression (CComma exprs _) = putStrLn "Analyzing CExpression" >> putStrLn "Analyzing CComma" >> mapM_ analyzeCExpression exprs
CAssign CAssignOp (CExpression a) (CExpression a) a  
CCond (CExpression a) (Maybe (CExpression a)) (CExpression a) a  
CBinary CBinaryOp (CExpression a) (CExpression a) a  
CCast (CDeclaration a) (CExpression a) a     
CUnary CUnaryOp (CExpression a) a    
CSizeofExpr (CExpression a) a    
CSizeofType (CDeclaration a) a   
CAlignofExpr (CExpression a) a   
CAlignofType (CDeclaration a) a  
CComplexReal (CExpression a) a   
CComplexImag (CExpression a) a   
CIndex (CExpression a) (CExpression a) a     
CCall (CExpression a) [CExpression a] a  
CMember (CExpression a) Ident Bool a     
CVar Ident a     
CConst (CConstant a)    
integer, character, floating point and string constants
CCompoundLit (CDeclaration a) (CInitializerList a) a    
C99 compound literal
CStatExpr (CStatement a) a  
GNU C compound statement as expr
CLabAddrExpr Ident a    
GNU C address of label
CBuiltinExpr (CBuiltinThing a)


Right (CTranslUnit
[ CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") (Just []) [] ) )] [] )
, CDeclExt (CDecl [CStorageSpec (CExtern ),CTypeSpec (CVoidType )] [(Just (CDeclr (Just "insert") [CFunDeclr (Right ([CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr Nothing [CPtrDeclr [] ] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] ,CDecl [CTypeSpec (CIntType )] [] ],False)) [] ] Nothing [] ),Nothing,Nothing)] )
, CFDefExt (CFunDef [CTypeSpec (CIntType )] (CDeclr (Just "main") [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType )] [(Just (CDeclr (Just "argc") [] Nothing [] ),Nothing,Nothing)] ,CDecl [CTypeQual (CConstQual ),CTypeSpec (CCharType )] [(Just (CDeclr (Just "argv") [CArrDeclr [] (CNoArrSize False) ,CPtrDeclr [] ] Nothing [] ),Nothing,Nothing)] ],False)) [] ] Nothing [] ) [] (CCompound []
	[CBlockDecl (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr (Just "d1") [] Nothing [] ),Nothing,Nothing)] )
	,CBlockStmt (CExpr (Just (CCall (CVar "insert" ) [CUnary CAdrOp (CVar "d1" ) ,CConst (CIntConst 4 )] )) )
	,CBlockStmt (CReturn (Just (CConst (CIntConst 0))))]))
])
-}
