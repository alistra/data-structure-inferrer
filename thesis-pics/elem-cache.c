struct ds_new {
	ds old;
	int max_elem;
}

void insert_d(ds_new *d, int elem){

	if (elem > d->max_elem)
		d->max_elem = elem;

	old_insert_d(d->old, elem);
}

void delete_d(ds_new *d, int elem){

	old_delete_d(d->old, elem)

	if (elem == d->max_elem)
		d->max_elem = old_max_d(d->old);
}

void update_d(ds_new *d, int old, int new){

	if (new > d->max_elem) {
		d->max_elem = new;
	}

	old_update_d(d->old, old, new);
}

int max(ds_new *d, int elem){

	return d->max_elem;
}

void delete_max_d(ds_new *d){

	old_delete_max_d(d->old)

	d->max_elem = old_max_d(d->old);
}
