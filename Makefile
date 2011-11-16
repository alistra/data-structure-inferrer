thesis:
	rubber -d thesis.tex

clean:
	-rm thesis.aux thesis.log thesis.toc
	-rm *.hi *.o
	-rm Il/Lexer.hs Il/Parser.hs Il/grammar.log

mrproper: clean
	-rm dsinf
	-rm thesis.pdf
