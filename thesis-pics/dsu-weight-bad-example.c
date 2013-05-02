#include <ds.h>

const int n = 4096;
const int x = 10;

int main(int argc, char **argv)
{
	ds d = init_d();

	for(int i = 0; i < n; i++)
		insert_d(d, i, 0);

	delete_max_d(d, 1);
	delete_min_d(d, 1);

	for(int i = 0; i < x; i++)
		search_d(d, i, x);

}
