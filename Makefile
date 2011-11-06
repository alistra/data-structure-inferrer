analyzer: Il/Lexer.hs Il/Parser.hs
	ghci -O Tests.hs

Il/Lexer.hs: Il/Lexer.x
	alex Il/Lexer.x

Il/Parser.hs: Il/Parser.y
	happy -iIl/grammar.log Il/Parser.y

clean:
	rm Il/Lexer.hs Il/Parser.hs Il/grammar.log thesis.log

fixme:	
	grep FIXME *hs

doc:	*.hs
	haddock -t "Data Structure Inferrer" -o doc -h *.hs
	git checkout gh-pages
	cp -r doc/* .
	git add .
	git commit -a -m "Automated doc push"
	git push origin gh-pages
	git checkout master
