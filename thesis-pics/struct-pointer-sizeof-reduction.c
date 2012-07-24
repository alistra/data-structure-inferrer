#define length (3 * sizeof(int) + 128)

int cmp(char *p1, char *p2) {

	return (int)p1 - (int)p2;
}

int main(int argc, char **argv) {

	char p[length];
	...
	ds d;
	d = dsinit(cmp);
	insert_d(d, p);
}

