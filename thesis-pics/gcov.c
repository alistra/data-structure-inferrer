typedef int dstype;
#include "ds.h"

int main(int argc, const char *argv[])
{

	ds d = init_d();

	for(int i = 0; i < 402341; i++)
		insert_d(d, i);

	search_d(d, 4);
}
