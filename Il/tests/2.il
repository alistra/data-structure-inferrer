ds d1
ds d2

for(i = 0; i < 20; i++)
{
	insert(d1, i)
}

for(i = 0; i < 10; i++)
{
	insert(d2, i)
	update(d1, i, 2*i)
}

for(i = 0; i < 5; i++)
{
	delete_max(d1)
	delete_max(d2)
}

max(d1)
