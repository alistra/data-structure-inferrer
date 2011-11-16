#include<stdlib.h>

typedef int elem;

struct linkedlist {
	elem e;
	struct linkedlist *next;
};

struct ds {
	struct linkedlist *list;
	int elem_count;
};

struct ds* new()
{
	struct ds *d = (struct ds*) malloc(sizeof (struct ds));
	d->list = 0;
	d->elem_count = 0;
	return d;
}


elem insert(struct ds *d, elem e)
{
	struct linkedlist *l = malloc(sizeof (struct linkedlist));
	l->e = e;
	l->next = 0;
	//TODO rest
}

