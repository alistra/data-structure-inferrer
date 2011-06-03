ds = DS
ds2 = DS

for(i = 0; i < 20; i++)
{
	insert(ds, i)
}

for(i = 0; i < 10; i++)
{
	insert(ds2, i)
	update(ds, i, 2*i)
}

for(i = 0; i < 5; i++)
{
	delete_max(ds)
	delete_max(ds2)
}

max(ds)
