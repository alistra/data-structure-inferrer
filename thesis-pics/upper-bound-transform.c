#include <stdio.h>

int main(int argc, char ** argv)
{

	ds d = ds_init();
	
	for (int i = 0; i < 1024; i++)
		insert_d(d, f(i));


	g();
	delete_d(d, 1337);
	//part of the code not inserting any more elements


	return 0;
}
