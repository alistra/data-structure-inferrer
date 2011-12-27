IL=Il/Analyzer.hs Il/Typechecker.hs Il/AST.hs
C=C/Analyzer.hs C/Functions.hs
SRC=Advice.hs AllStructures.hs Recommend.hs Analyzer.hs Defs/*.hs ${C}
LEXPAR=Il/Lexer.hs Il/Parser.hs
CDOPTS=-package-conf cabal-dev/packages-7.2.2.conf

dsinf: ${LEXPAR} ${SRC} Main.hs
	ghc --make ${CDOPTS} -o dsinf -O Main.hs

tests: ${LEXPAR} ${SRC} Tests.hs
	ghci ${CDOPTS} -O Tests.hs

Il/Lexer.hs: Il/Lexer.x
	alex -g Il/Lexer.x

Il/Parser.hs: Il/Parser.y
	happy -g Il/Parser.y

todo:
	-egrep "(TODO|HMMM|FIXME|BUG|HACK|STUB|undefined)" ${SRC}

clean:
	cabal-dev clean
	rm -f Il/Lexer.hs Il/Parser.hs
	rm -f thesis.log thesis.aux thesis.toc

cleanbin: clean
	rm -f dsinf
	rm -f thesis.pdf

doc:	${LEXPAR} ${SRC} Main.hs
	haddock --ignore-all-exports -t"Data Structure Inferrer" -o doc -h Main.hs --optghc="-package-conf cabal-dev/packages-7.2.2.conf"
	git checkout gh-pages
	cp -r doc/* .
	git add .
	git commit -a -m "Automated doc push"
	git push origin gh-pages
	git checkout master
