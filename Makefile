analyzer: 	Defs/AST.hs Defs/Structures.hs Il/Lexer.hs Il/Parser.hs Analyzer.hs
	ghci -O Analyzer.hs 

Il/Lexer.hs: Il/Lexer.x
	alex Il/Lexer.x

Il/Parser.hs: Il/Parser.y
	happy -iIl/grammar.log Il/Parser.y
