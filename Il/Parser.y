{
module Il.Parser where

import Il.Lexer
import Il.AST
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
	DS		{ (_,TkDS)	}
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
	LEqual		{ (_,TkLEqual)	}
	Less		{ (_,TkLess)	}
	LParen          { (_,TkLParen) 	}
	Minus		{ (_,TkMinus)  	}
	Mul           	{ (_,TkMul) 	}
	Name		{ (_,TkName $$)	}
	Newline		{ (_,TkNewline) }
	Not		{ (_,TkNot)     }
	Null		{ (_,TkNull)	}
	Or              { (_,TkOr)   	}
	Plus            { (_,TkPlus)  	}
	RCParen         { (_,TkRCParen) }
	RParen          { (_,TkRParen) 	}
	Semicolon       { (_,TkSemicolon) }
	Then		{ (_,TkThen) 	}
	True		{ (_,TkTrue)	}
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


expr :: { Term }
expr:		Name Assign valexpr							{ Assign $1 $3 }
		| Name Assign DS							{ DSInit $1 }
		| block									{ $1 }
		| If valexpr Then expr Else expr 					{ If $2 $4 $6 }
		| If valexpr Newline Then expr Newline Else expr 			{ If $2 $5 $8 }
		| For LParen expr Semicolon valexpr Semicolon expr RParen expr 		{ For $3 $5 $7 $9 }
		| For LParen expr Semicolon valexpr Semicolon expr RParen Newline expr 	{ For $3 $5 $7 $10 }
		| While LParen expr RParen expr						{ While $3 $5 }
		| While LParen expr RParen Newline expr					{ While $3 $6 }
		| shexpr								{ $1 }

shexpr :: { Term }
shexpr:		Inc valexpr				{ Inc $2 }
		| valexpr Inc				{ Inc $1 }
		| Dec valexpr				{ Dec $2 }
		| valexpr Dec				{ Dec $1 }
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
		| shexpr				{ $1 }
       		
block :: { Term }
block:		LCParen exprlist RCParen		{ Block $2 }
     		| LCParen RCParen			{ Block [] }

exprlist :: { [Term] }
exprlist:	block exprlist				{ $1:$2 } 
		| expr Newline exprlist			{ $1:$3 }
		| expr					{ [$1] }

commaseparatedlist :: { [Term] }
commaseparatedlist: 	valexpr Comma commaseparatedlist { $1:$3 }
		  	| valexpr			 { [$1] }
		  	| 				 { [] }
{

parseError :: [Token] -> a
parseError (((line,col),t):xs) = error $ "Parse error at line " ++ (show line) ++ ", column " ++ (show col)
parseError [] = error "Parse error at the end"

}
