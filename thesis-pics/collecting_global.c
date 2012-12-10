typedef int dstype;
#include "../../dsimp/ds.h"
ds global_ds;

int main()
{
	insert_d(global_ds);
	delete_max_d(global_ds);
	printf("%d\n", max_d(global_ds));
}
