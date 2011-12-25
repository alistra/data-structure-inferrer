int main()
{
	ds d;

	for(int i = 0; i < 20; i++)
	{
		insert(d, i);
	}

	printf("%d\n", max(d));
}
