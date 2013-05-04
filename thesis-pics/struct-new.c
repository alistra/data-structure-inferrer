enum person_orders {
	height_order = 0,
	weight_order,
	person_orders_count
};

int main(int argc, const char *argv[])
{
	ds d = init_d(person_orders_count, cmp_height, cmp_weight);

	....

	search_d(d, 4, height_order);

	max_d(d, weight_order);
}

