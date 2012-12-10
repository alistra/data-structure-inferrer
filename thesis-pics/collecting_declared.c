typedef int dstype;
#include "../../dsimp/ds.h"

int main()
{
	ds declared_ds;
	insert_d(declared_ds);
	delete_max_d(declared_ds);
	printf("%d\n", max_d(declared_ds));
}
