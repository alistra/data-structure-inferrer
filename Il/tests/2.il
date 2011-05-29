ds = DS

for(i = 0; i < 20; i++)
{
	insert(ds, i)
}

for(i = 0; i < 10; i++)
{
	update(ds, i, 2*i)
}

for(i = 0; i < 5; i++)
{
	print(delete_max(ds))
}

print(max(ds))
