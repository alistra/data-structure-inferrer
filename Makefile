analyzer: 	Structures.hs Lexer.hs AST.hs Analyzer.hs Parser.hs
	ghci -O Structures.hs Lexer.hs AST.hs Analyzer.hs Parser.hs 

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy -igrammar.log Parser.y

clean:
	rm *.hi *.o grammar.log
