typedef int dstype;
#include "../../dsimp/ds.h"

void f(ds parameter_ds)
{
	insert_d(parameter_ds, 2);
	delete_max_d(parameter_ds);
	printf("%d\n", max_d(parameter_ds));
}

int main()
{
	ds declared_ds;
	f(declared_ds);
}

//yielding: 	[(parameter_ds, insert_d), (parameter_ds, delete_max_d),
//		(parameter_ds, max_d)]
