{
module Il.Lexer where

import Prelude hiding (lex)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :- 
	(\n)+				{ \p s -> tokenWithPos p TkNewline }
	($white # \n)+			;
    	$digit+				{ \p s -> tokenWithPos p (TkInt (read s)) }
	and				{ \p s -> tokenWithPos p TkAnd }
	if				{ \p s -> tokenWithPos p TkIf }
	then				{ \p s -> tokenWithPos p TkThen }
	else				{ \p s -> tokenWithPos p TkElse }
	false				{ \p s -> tokenWithPos p TkFalse }
	true				{ \p s -> tokenWithPos p TkTrue }
	for				{ \p s -> tokenWithPos p TkFor }
	while				{ \p s -> tokenWithPos p TkWhile }
	null				{ \p s -> tokenWithPos p TkNull }
	or				{ \p s -> tokenWithPos p TkOr }
	ds				{ \p s -> tokenWithPos p TkDs }
	dsElem				{ \p s -> tokenWithPos p TkDsElem }
	int				{ \p s -> tokenWithPos p TkTInt }
	bool				{ \p s -> tokenWithPos p TkTBool }
    	"++"				{ \p s -> tokenWithPos p TkInc }
    	"+"				{ \p s -> tokenWithPos p TkPlus }
    	"--"				{ \p s -> tokenWithPos p TkDec }
    	"-"				{ \p s -> tokenWithPos p TkMinus }
    	"*"				{ \p s -> tokenWithPos p TkMul }
    	"/"				{ \p s -> tokenWithPos p TkDiv }
	"!"				{ \p s -> tokenWithPos p TkNot }
	","				{ \p s -> tokenWithPos p TkComma }
	";"				{ \p s -> tokenWithPos p TkSemicolon }
    	"."				{ \p s -> tokenWithPos p TkDot }
	"=="				{ \p s -> tokenWithPos p TkEquals }
	">="				{ \p s -> tokenWithPos p TkGEqual }
	"<="				{ \p s -> tokenWithPos p TkLEqual }
	"="				{ \p s -> tokenWithPos p TkAssign }
    	"("				{ \p s -> tokenWithPos p TkLParen }
	"{"				{ \p s -> tokenWithPos p TkLCParen }
	"["				{ \p s -> tokenWithPos p TkLSParen }
	"<"				{ \p s -> tokenWithPos p TkLess }
	")"				{ \p s -> tokenWithPos p TkRParen }      
	"}"				{ \p s -> tokenWithPos p TkRCParen }
	"]"				{ \p s -> tokenWithPos p TkRSParen }
	">"				{ \p s -> tokenWithPos p TkGreater }
	$alpha (_ | $digit | $alpha)* 	{ \p s -> tokenWithPos p (TkName s) }
{                                                 
data BaseToken = TkAnd 
	| TkAssign
	| TkColon 
	| TkComma 
	| TkDec
	| TkDiv
	| TkDot 
	| TkDs
	| TkDsElem
	| TkElse
	| TkEquals
	| TkFalse
	| TkFor
	| TkGEqual
	| TkGreater
	| TkIf
	| TkInc
	| TkInt Int
	| TkLAParen
	| TkLCParen
	| TkLSParen
	| TkLEqual
	| TkLess
	| TkLParen 
	| TkMinus                              
	| TkMul
	| TkName String                     
	| TkNewline
	| TkNot
	| TkNull
	| TkOr 
	| TkPlus 
	| TkRAParen
	| TkRCParen
	| TkRSParen
	| TkRParen
	| TkSemicolon 
	| TkThen
	| TkTrue
	| TkTInt
	| TkTBool
	| TkWhile
	deriving (Show, Eq)
          
type Token = ((Int,Int), BaseToken)

tokenWithPos :: AlexPosn -> BaseToken -> Token
tokenWithPos (AlexPn _ line col) t  = ((line,col),t)

lex = alexScanTokens
}
