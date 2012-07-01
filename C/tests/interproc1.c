typedef  int dstype;
#include "../../dsimp/ds.h"


void f(ds d) {
	insert_d(d, 5);
}

void d(ds d) {
	delete_d(d, 5);
}

void m(ds d) {
	delmax_d(d);
}

void f2(ds d) {
	insert_d(d, 5);
}

void d2(ds d) {
	delete_d(d, 5);
}

int main()
{
	ds ds1;
	f(ds1);
	d(ds1);
	m(ds1);
	ds ds2;
	f2(ds2);
	d2(ds2);
}
