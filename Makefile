IL=Il/Analyzer.hs Il/Typechecker.hs Il/AST.hs ${LEXPAR}
C=C/Analyzer.hs C/Functions.hs
SRC=Main.hs Advice.hs AllStructures.hs Recommend.hs Analyzer.hs CompileDriver.hs Defs/*.hs ${C}
LEXPAR=Il/Lexer.hs Il/Parser.hs

CDOPTS=-package-conf cabal-dev/packages-7.2.2.conf

dsinf:  ${SRC}
	ghc --make ${CDOPTS} -o dsinf -O Main.hs

tests:  ${SRC} Tests.hs
	ghci ${CDOPTS} -O Tests.hs

Il/Lexer.hs: Il/Lexer.x
	alex -g Il/Lexer.x

Il/Parser.hs: Il/Parser.y
	happy -g Il/Parser.y

todo:
	-egrep "(TODO|HMMM|FIXME|BUG|HACK|STUB|undefined)" ${SRC}

doc:	${SRC}
	haddock --ignore-all-exports -t"Data Structure Inferrer" -o doc -h Main.hs --optghc="-package-conf cabal-dev/packages-7.2.2.conf"
	git checkout gh-pages
	cp -r doc/* .
	git add .
	git commit -a -m "Automated doc push"
	git push origin gh-pages
	git checkout master

thesis:
	rubber -d thesis.tex
	make texclean

clean:	texclean
	rm -f Il/Lexer.hs Il/Parser.hs
	rm -f *.o *.hi C/*.o C/*.hi Defs/*.o Defs/*.hi

texclean:
	rm -f thesis.aux thesis.log thesis.toc

binclean: clean
	rm -f dsinf

pdfclean: clean
	rm -f thesis.pdf
