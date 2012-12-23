typedef int dstype;
#include "../../dsimp/ds.h"

void f(ds parameter_ds)
{
	delete_max_d(parameter_ds);
	printf("%d\n", max_d(parameter_ds));
}

int main()
{
	ds ds1, ds2;
	insert_d(d1, 5);
	insert_d(d2, 7);

	f(ds1);
	f(ds2);
	printf("%d\n", max_d(ds1));
}
