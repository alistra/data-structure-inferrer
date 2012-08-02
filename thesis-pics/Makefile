gcov:
	gcc -fprofile-arcs -ftest-coverage gcov.c ../dsimp/null.c -std=c99
	./a.out
	gcov gcov.c

clean:
	rm -f *gcov *gcno *gcda a.out
