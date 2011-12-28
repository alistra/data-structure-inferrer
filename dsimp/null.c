typedef int dstype;

#include "ds.h"
#include <stdlib.h>

struct ds {
	dstype el;
};

ds init_d()
{
	return malloc(sizeof (struct ds)); //FIXME not checking malloc
}

void insert_d(ds d, dstype e)
{
	d->el = e;
}

void update_d(ds d, dstype o, dstype n)
{
	if (d->el == o)
		d->el = n;
}

void delete_d(ds d, dstype e)
{
	if (d->el == e)
		d->el = (dstype) 0;
}

void delmax_d(ds d)
{
	d->el = (dstype) 0;
}

void delmin_d(ds d)
{
	d->el = (dstype) 0;
}

int search_d(ds d, dstype e)
{
	return d->el == e;
}

dstype max_d(ds d)
{
	return d->el;
}

dstype min_d(ds d)
{
	return d->el;
}
