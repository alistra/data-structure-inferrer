extern struct ds;

extern void insert(struct ds*, int);

int main(int argc, const char *argv[])
{
	struct ds d1;
	insert(d1, 4);
	return 0;
}
/*
Right (CTranslUnit
[ CDeclExt (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") (Just []) [] ) )] [] )
, CDeclExt (CDecl [CStorageSpec (CExtern ),CTypeSpec (CVoidType )] [(Just (CDeclr (Just "insert") [CFunDeclr (Right ([CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr Nothing [CPtrDeclr [] ] Nothing [] (OnlyPos <no file> (<no file>,-1))),Nothing,Nothing)] ,CDecl [CTypeSpec (CIntType )] [] ],False)) [] ] Nothing [] ),Nothing,Nothing)] )
, CFDefExt (CFunDef [CTypeSpec (CIntType )] (CDeclr (Just "main") [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType )] [(Just (CDeclr (Just "argc") [] Nothing [] ),Nothing,Nothing)] ,CDecl [CTypeQual (CConstQual ),CTypeSpec (CCharType )] [(Just (CDeclr (Just "argv") [CArrDeclr [] (CNoArrSize False) ,CPtrDeclr [] ] Nothing [] ),Nothing,Nothing)] ],False)) [] ] Nothing [] ) [] (CCompound []
	[CBlockDecl (CDecl [CTypeSpec (CSUType (CStruct CStructTag (Just "ds") Nothing [] ) )] [(Just (CDeclr (Just "d1") [] Nothing [] ),Nothing,Nothing)] )
	,CBlockStmt (CExpr (Just (CCall (CVar "insert" ) [CUnary CAdrOp (CVar "d1" ) ,CConst (CIntConst 4 )] )) )
	,CBlockStmt (CReturn (Just (CConst (CIntConst 0))))]))
])
*/
