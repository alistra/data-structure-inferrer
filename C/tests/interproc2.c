typedef  int dstype;
#include "../../dsimp/ds.h"


void f(ds d, ds d2) {
	insert_d(d, 5);
	delete_d(d, 5);
	insert_d(d2, 5);
	delete_d(d2, 5);
	delmax_d(d2);
}


int main()
{
	ds ds1;
	ds ds2;
	f(ds1, ds2);
}
