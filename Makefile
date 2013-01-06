IL=Il/Analyzer.hs Il/Typechecker.hs Il/AST.hs ${LEXPAR}
C=C/Analyzer.hs C/Functions.hs
SRC=Main.hs Advice.hs AllStructures.hs Recommend.hs Analyzer.hs CompileDriver.hs Defs/*.hs ${C}
LEXPAR=Il/Lexer.hs Il/Parser.hs

CDOPTS=-package-conf cabal-dev/packages-7.2.2.conf

dsinf:  ${SRC}
	cabal-dev install --only-dependencies
	cabal-dev configure
	cabal-dev build
	install -m755 dist/build/dsinf/dsinf .


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

thesis: thesis.tex thesis-pics/*
	rubber -d thesis.tex
	rubber --clean thesis.tex
	-mplayer -really-quiet /opt/WorldOfGoo/res/sounds/fling01.ogg

clean:
	rm -f Il/Lexer.hs Il/Parser.hs
	rm -f *.o *.hi C/*.o C/*.hi Defs/*.o Defs/*.hi

binclean: clean
	rm -f dsinf

pdfclean: clean
	rm -f thesis.pdf
