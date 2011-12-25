int main()
{
	ds d1;
	dselem e;

	for(int i = 0; i < 20; i++)
	{
		insert(d1, i);
		e = search(d1, i);
		update(e, 2*i);
	}

	printf("%d\n", max(d1));
}
