int main()
{
	ds d1
	int i
	dselem e
	for(i = 0; i < 20; i++)
	{
		insert(d1, i)
		e = search(d1, i)
		update(e, 2*i)
	}

	max(d1)
}
