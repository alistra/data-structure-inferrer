typedef  int dstype;
#include "../../dsimp/ds.h"


void f(ds d) {
	g(d);
}

void g(ds d) {
	h(d);
}

void h(ds d) {
	insert_d(d, 5);
	delete_d(d, 5);
	delmax_d(d);
}
int main()
{
	ds ds1;
	f(ds1);
}
