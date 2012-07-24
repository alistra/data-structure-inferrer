#include <ds.h>

int main(int argc, char **argv)
{
	ds d = init_d();

	for(int i = 0; i < 42042323; i++)
		insert_d(d, i);

	printf("DEBUG: %d", delete_max_d(d));


	for(int i = 0; i < 42042323; i++)
		search_d(d, i);
}
