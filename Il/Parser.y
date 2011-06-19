{
module Il.Parser where

import Il.Lexer
import Defs.AST
import Defs.Common

import Prelude hiding (True, False)
}

%name parse exprlist

%tokentype { Token }
%error     { parseError }

%token
	And             { (_,TkAnd)	}
	Assign          { (_,TkAssign) 	}
	Comma           { (_,TkComma)  	}
	Dec		{ (_,TkDec) 	}
	Div           	{ (_,TkDiv) 	}
	Ds		{ (_,TkDs)	}
	DsElem		{ (_,TkDsElem)	}
	Else		{ (_,TkElse)   	}
	Equals          { (_,TkEquals) 	}
	False		{ (_,TkFalse)	}
	For		{ (_,TkFor)	}
	GEqual		{ (_,TkGEqual)	}
	Greater		{ (_,TkGreater)	}
	If		{ (_,TkIf)     	}
	Inc		{ (_,TkInc) 	}
	Int		{ (_,TkInt $$) 	}
	LCParen         { (_,TkLCParen)	}
	LSParen		{ (_,TkLSParen) }
	LEqual		{ (_,TkLEqual)	}
	LParen          { (_,TkLParen) 	}
	Less		{ (_,TkLess)	}
	Minus		{ (_,TkMinus)  	}
	Mul           	{ (_,TkMul) 	}
	Name		{ (_,TkName $$)	}
	Newline		{ (_,TkNewline) }
	Not		{ (_,TkNot)     }
	Null		{ (_,TkNull)	}
	Or              { (_,TkOr)   	}
	Plus            { (_,TkPlus)  	}
	RCParen         { (_,TkRCParen) }
	RSParen		{ (_,TkRSParen) }
	RParen          { (_,TkRParen) 	}
	Semicolon       { (_,TkSemicolon)}
	Then		{ (_,TkThen) 	}
	True		{ (_,TkTrue)	}
	TInt		{ (_,TkTInt)	}
	TBool		{ (_,TkTBool)	}
	While		{ (_,TkWhile)	}

%left Else RParen
%nonassoc Not
%nonassoc Assign
%left And Or
%nonassoc Less Greater GEqual LEqual Equals
%left Plus Minus
%left Mul Div
%nonassoc Inc Dec
%nonassoc Newline
%%                              


type :: { Type }
type:		TInt				{ TInt }
    		| TBool				{ TBool }
		| Ds				{ Ds }
		| DsElem			{ DsElem }
		| LSParen trecordintern RSParen { TRec $2 }

trecordintern :: { [(Name, Type)] }
trecordintern:	trecordpair Comma trecordintern { $1 : $3 }
	     	| trecordpair 			{ [$1] }

trecordpair :: { (Name, Type) }
trecordpair:	type Name 			{ ($2, $1) }

expr :: { Term }
expr:		Name Assign valexpr							{ Assign $1 $3 }
		| type Name Assign valexpr						{ InitAssign $2 $4 $1 }
		| type Name								{ VarInit $2 $1 }
		| block									{ $1 }
		| If valexpr Then expr Else expr 					{ If $2 $4 $6 }
		| If valexpr Newline Then expr Newline Else expr 			{ If $2 $5 $8 }
		| For LParen expr Semicolon valexpr Semicolon expr RParen expr 		{ While $5 (Block [$3, $7, $9]) }
		| For LParen expr Semicolon valexpr Semicolon expr RParen Newline expr 	{ While $5 (Block [$3, $7, $10]) }
		| While LParen valexpr RParen expr					{ While $3 $5 }
		| While LParen valexpr RParen Newline expr				{ While $3 $6 }
		| shexpr								{ $1 }

shexpr :: { Term }
shexpr:		Inc Name				{ Inc $2 }
		| Name Inc				{ Inc $1 }
		| Dec Name				{ Dec $2 }
		| Name Dec				{ Dec $1 }
		| Name LParen commaseparatedlist RParen { Funcall $1 $3 }

valexpr :: { Term }
valexpr:	Name					{ Var $1 }
    		| Null					{ Int 0 }
    		| Int					{ Int $1 }
		| False					{ Int 0 }
		| True					{ Int 1 }
		| Not valexpr				{ Not $2 }
		| valexpr And valexpr			{ And $1 $3 }
		| valexpr Or valexpr			{ Or $1 $3 }
		| valexpr Plus valexpr			{ Sum $1 $3 }
		| valexpr Minus valexpr			{ Sub $1 $3 }
		| valexpr Mul valexpr			{ Mul $1 $3 }
		| valexpr Div valexpr			{ Div $1 $3 }
		| valexpr Equals valexpr		{ Eq $1 $3 }
		| valexpr LEqual valexpr		{ Leq $1 $3 }
		| valexpr GEqual valexpr		{ Geq $1 $3 }
		| valexpr Greater valexpr		{ Gt $1 $3 }
		| valexpr Less valexpr			{ Lt $1 $3 }
		| LParen valexpr RParen			{ $2 }
		| record				{ $1 }
		| shexpr				{ $1 }
       		
record :: { Term }
record:		LSParen RSParen 			{ Record [] }
      		| LSParen recordintern RSParen		{ Record $2 }

recordintern :: { [(Name, Term)] }
recordintern: 	recordpair Comma recordintern		{ $1 : $3 }
	    	| recordpair				{ [$1] }


recordpair :: { (Name, Term) }
recordpair:	Name Assign valexpr			{ ($1, $3) }

block :: { Term }
block:		LCParen Newline exprlist RCParen	{ Block $3 }
		| LCParen exprlist RCParen		{ Block $2 }
     		| LCParen RCParen			{ Block [] }
     		| LCParen Newline RCParen		{ Block [] }

exprlist :: { [Term] }
exprlist:	block exprlist				{ $1:$2 } 
		| expr Newline exprlist			{ $1:$3 }
		| expr Newline				{ [$1] }
		| expr 					{ [$1] }

commaseparatedlist :: { [Term] }
commaseparatedlist: 	valexpr Comma commaseparatedlist { $1:$3 }
		  	| valexpr			 { [$1] }
		  	| 				 { [] }
{

parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"

}
