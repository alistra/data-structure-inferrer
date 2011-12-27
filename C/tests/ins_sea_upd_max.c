typedef int dstype;
#include "../../dsimp/ds.h"
int main()
{
	ds d1;

	for(int i = 0; i < 20; i++)
	{
		insert_d(d1, i);
		printf("%d\n", search_d(d1, i));
		update_d(d1, i, 2*i);
	}

	printf("%d\n", max_d(d1));
}
