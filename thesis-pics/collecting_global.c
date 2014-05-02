typedef int dstype;
#include "../../dsimp/ds.h"
ds global_ds;
ds global_ds2;

int main()
{
	insert_d(global_ds, 5);
	delete_max_d(global_ds);
	printf("%d\n", max_d(global_ds));
	insert_d(global_ds2, 7);
	delete_d(global_ds2, 5);
}

//yielding 	[(global_ds, insert_d), (global_ds, delete_max_d),
//		 (global_ds, max_d),
//	  	 (global_ds2, indert_d), (global_ds2, delete_d)]

