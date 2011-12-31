# About the project
This project is meant to be a compiler feature/wrapper that analyzes your code and chooses the best data structure depending on your source code. It analyzes the functions used on a wildcard data structure and chooses the type of structure that minimizes the time complexity on compile time.

It can also work as a standalone source code analyzer. It recommends you the best data structure and advises which operation to cut out, to get a better performance.

This project is meant to act as a standard library of data structures, but the programmer doesn't have to know which structures are available and which ones match to the current task. The program simply does that for the programmer.

It supports C language and will hopefully support some other languages too.

# Usage

	dsinf [-OPT1 [VAL1] [-OPT2 [VAL2] [...]] [-- [CCOPTS]]
	  -o file  --output=file  Output file
	  -r       --recommend    Give recommendations about the data structure in the supplied code (default)
	  -a       --advice       Give advice about the data structure in the supplied code
	  -c       --compile      Compile the code with recommended structure linked
	  -i       --inline       Inline the code implementing the recommended structure in the supplied code
	  -v       --verbose      Enable verbose messages
	  -h       --help         Show help

	CCOPTS are passed directly to the compiler

# Example

For the following C file:

	//example.c
	typedef int dstype;
	#include <ds.h>
	int main()
	{
		ds d1;

		for(int i = 0; i < 20; i++)
		{
			insert_d(d1, i);
			printf("%d\n", search_d(d1, i));
			printf("%d\n", search_d(d1, 20 + i));
			update_d(d1, i, 2*i);
		}

		printf("%d\n", max_d(d1));
	}

you'll invoke:

	dsinf -c example.c

and it will automatically compile the program using the matching library with the data structure implementation (here - red-black trees with maximal element cache).
