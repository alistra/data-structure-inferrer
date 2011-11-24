SRC=Advice.hs AllStructures.hs Analyzer.hs CAnalyzer.hs Recommend.hs Typechecker.hs
LEXPAR=Il/Lexer.hs Il/Parser.hs

dsinf: ${LEXPAR} ${SRC} Main.hs
	ghc --make -o dsinf -O Main.hs

tests: ${LEXPAR} ${SRC} Tests.hs
	ghci -O Tests.hs

Il/Lexer.hs: Il/Lexer.x
	alex Il/Lexer.x

Il/Parser.hs: Il/Parser.y
	happy -iIl/grammar.log Il/Parser.y

clean:
	-rm Il/Lexer.hs -rm Il/Parser.hs -rm Il/grammar.log
	-rm thesis.log thesis.aux thesis.toc
	-rm *.hi -rm *.o

cleanbin: clean
	-rm dsinf
	-rm thesis.pdf

doc:	*.hs
	haddock -t "Data Structure Inferrer" -o doc -h Main.hs
	git checkout gh-pages
	cp -r doc/* .
	git add .
	git commit -a -m "Automated doc push"
	git push origin gh-pages
	git checkout master
