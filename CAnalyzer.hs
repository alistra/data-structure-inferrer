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

analyzeCStatement statement = putStrLn "Analyzing CStatement" >> print statement

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
