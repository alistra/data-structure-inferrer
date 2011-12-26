typedef int dstype;

#include "../../dsimp/ds.h"

int main()
{
	ds d1;
	ds d2;
	for(int i = 0; i < 20; i++)
	{
		insert_d(d1, i);
	}

	for(int i = 0; i < 10; i++)
	{
		insert_d(d2, i);
		update_d(d1, i, 2*i);
	}

	for(int i = 0; i < 5; i++)
	{
		delete_max_d(d1);
		delete_max_d(d2);
	}

	printf("%d\n", max_d(d1));
}
