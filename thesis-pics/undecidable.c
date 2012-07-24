#include <ds.h>

int main(int argc, char **argv)
{
	char c;
	ds d = init_d();
	while(1){
		scanf("%c", &c);
		switch(c){
			case 'i':
				insert_d(d, 5);
			case 'u':
				update_d(d, 5, 7);
			case 'd':
				delete_d(d, 5);
			case 'm':
				max_d(d);
			...
			//every other possible operation
		}
	}
}
