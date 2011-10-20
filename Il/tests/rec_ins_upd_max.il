int main()
{
	ds d1
	int i

	[int key, int val] rec = [key = 5, val = 1337]
	[int key, int val] rec2 = [key = 10, val = 1337]

	for(i = 0; i < 20; i++)
	{
		insert(d1, rec)
		update(d1, rec, rec2)
	}

	max(d1)
}
