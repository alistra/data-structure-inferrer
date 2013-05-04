int main(int argc, char **argv) {

	char p[sizeof(struct person)];
	...
	ds d;
	d = dsinit(cmp);
	insert_d(d, p);
}

