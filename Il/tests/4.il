int main()
{
	ds d1

	for(i = 0; i < 20; i++)
	{
		insert(d1, i)
		update(d1, i, 2*i)
	}

	print(max(d1))
}
