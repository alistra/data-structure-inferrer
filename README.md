# About the project
This project is meant to be a compiler feature that analyzes your code and chooses the best data structure depending on your source code. It analyzes the functions used on a data structure and chooses a type of structure that minimizes the time complexity. For now it works as a standalone code analyzer that prints the most appropriate structure.

# Examples

You can run tests by running runIlTests from the Tests.hs file. These tests (Il/tests subdirectory) are source code in a simple imperative language. The analyzer infers the best data structure for operations used in the test program.

	*Tests> runIlTests
	Test File Il/tests/1.il
	The recommended structure for ds is:
	Hashtable
	Test File Il/tests/2.il
	The recommended structure for ds is:
	Red-Black Trees with extreme element caching
	The recommended structure for ds2 is:
	Red-Black Trees with extreme element caching
	Test File Il/tests/3.il
	The recommended structure for ds is:
	Red-Black Trees with extreme element caching
	The recommended structure for ds2 is:
	Linked List with extreme element caching

# Low Level

For now you can get the best structures depending on operations used in your program (if you manually put it in the invocation):

	*AllStructures> recommendAllDs [InsertVal, ExtremalVal]
	[Fibonacci Heap,Binomial Heap]
	*AllStructures> recommendAllDs [InsertVal, FindByVal ]
	[Hashtable]
	*AllStructures> recommendAllDs [InsertVal, FindByVal, ExtremalVal]  
	[Red-Black Trees]

There's an advice mode which is formatted more nicely:

	*Advice> printAdvice [InsertVal, UpdateByVal, DeleteExtremalVal ]
	Currently, the recommended data structure is: Red-Black Trees
	You could use Hashtable, if you removed the following operations:
	* DeleteExtremalVal


