This project is meant to be a compiler feature, that analyzes your code and chooses the best data structure depending on your source code. It analyzes the functions used on a data structure and chooses a type of structure that minimizes the time complexity. For now it works as a standalone code analyzer, that prints the most appropriate structure.

For now you can get the best structures depending on operations used in your program (if you manually put it in the invocation):

	*AllStructures> recommendAllDs [InsertVal, ExtremalVal]
	[Fibonacci Heap,Binomial Heap]
	*AllStructures> recommendAllDs [InsertVal, FindByVal ]
	[Hashtable]
	*AllStructures> recommendAllDs [InsertVal, FindByVal, ExtremalVal]  
	[Red-Black Trees]

There's an advice mode which is formatted more nicely:

	*Advice> printAdvice [InsertVal, UpdateByVal, DeleteExtremalVal ]
	Currently recommended data structure is: Red-Black Trees
	You could use Hashtable, if you removed the following operations:
	* DeleteExtremalVal

There's also a simple imperative language (Il) with a parser, lexer and analyzer for it in the Il/ folder.
