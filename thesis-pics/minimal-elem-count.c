#include <stdio.h>

int main(int argc, char ** argv)
{

	ds d = ds_init();
	
	for (int i = 0; i < 100; i++)
		insert_d(d, f(i));

	delete_d(d, 1337);
	
	for (int i = 0; i < 10000; i++)
		insert_d(d, f(i));

	delete_max_d(d);

	return 0;
}
