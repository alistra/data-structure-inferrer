#include <ds.h>
#include "turing.h"

int main(int argc, char **argv)
{
	ds d = init_d();
	insert_d(d, 6);

	turing_machine t;
	/*
	 * [...] definition of the turing machine here
	 */

	if (turing_machine_accepts(t, 2342)) {
		printf("accepted: %d", delete_max_d(d));
	}
}
