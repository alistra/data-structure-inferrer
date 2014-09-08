typedef int dstype;
#include "../../dsimp/ds.h"

void f(ds parameter_ds)
{
	delete_max_d(parameter_ds);
	printf("%d\n", min_d(parameter_ds));
}

int main()
{
	ds ds1, ds2, ds3;
	insert_d(d1, 5);
	insert_d(d2, 7);

	f(ds1);
	f(ds2);
	printf("%d\n", max_d(ds1));

	ds3 = ds2;
	update_d(ds3, 5, 7);
}

//parameter rule yields:
//		[(parameter_ds, delete_max_d), (parameter_ds, min_d)]
//
//declared rule yields:
//		[(ds1, insert_d), (ds1, max_d),
//		(ds2, insert_d),
//		(ds3, update_d)]
//
//grouping yields following groups:
// 		[(ds1, insert_d), (ds1, max_d), (parameter_ds,
// 		 delete_max_d), (parameter_ds, min_d)],
// 		[(ds2, insert_d), (parameter_ds, delete_max_d),
// 		 (parameter_ds, min_d), (ds3, update_d)]
