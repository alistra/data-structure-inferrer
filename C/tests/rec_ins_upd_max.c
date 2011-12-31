typedef struct keyval {
	int key;
	int val;
} dstype;
#include "../../dsimp/ds.h"

int main()
{
	ds d1;

	struct keyval rec, rec2;
	rec.key = 5;
       	rec.val = 1337;

	rec2.key = 10;
       	rec2.val = 1337;

	for(int i = 0; i < 20; i++)
	{
		insert_d(d1, rec);
		update_d(d1, rec, rec2);
	}

	printf("%d\n", max_d(d1));
}
