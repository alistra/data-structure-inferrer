#include <ds.h>
#include "person.h"

int cmp(struct person p1, struct person p2) {

	return p1.height - p2.heigth;
}

int main(int argc, char **argv) {

	struct person p;
	p.height = 143;
	p.weight = 213;
	p.age = 23;
	p.name = "James Pearseed"

	ds d;
	d = dsinit(cmp);
	insert_d(d, p);
}

