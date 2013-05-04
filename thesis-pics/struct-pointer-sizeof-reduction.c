int main(int argc, char **argv) {

	char p[sizeof(struct person)];
	...
	ds d = dsinit(cmp_height);
	insert_d(d, p);
}

