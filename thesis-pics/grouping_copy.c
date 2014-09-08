typedef int dstype;
#include "../../dsimp/ds.h"

int main()
{
	ds declared_ds;
	insert_d(declared_ds);

	ds copied_ds = declared_ds;
	printf("%d\n", max_d(copied_ds));
}