typedef int dstype;

#include "../../dsimp/ds.h"

int main()
{
	ds d;

	for(int i = 0; i < 20; i++)
	{
		insert_d(d, i);
	}

	printf("%d\n", max_d(d));
}
