#include <ds.h>
#include "person.h"

int main(int argc, char **argv) {

	struct person p;
	p.height = 143;
	p.weight = 213;
	p.age = 23;
	p.name = "James Pearseed"

	ds d = ds_init(cmp_height);
	insert_d(d, p);
}

