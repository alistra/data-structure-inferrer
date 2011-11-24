thesis:
	rubber -d thesis.tex

clean:
	rm -f thesis.aux thesis.log thesis.toc
	rm -f *.hi *.o
	rm -f Il/Lexer.hs Il/Parser.hs Il/grammar.log

cleanbin: clean
	rm -f dsinf
	rm -f thesis.pdf
