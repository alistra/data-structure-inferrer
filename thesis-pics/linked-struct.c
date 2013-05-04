struct person {
	int height;
	int weight;
	int age
	char name[128];
};

int cmp_height(struct person p1, struct person p2) {

		return p1.height - p2.heigth;
}


int cmp_weight(struct person p1, struct person p2) {

		return p1.weight - p2.heigth;
}
