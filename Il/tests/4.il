ds = DS

for(i = 0; i < 20; i++)
{
	insert(ds, i)
	update(ds, i, 2*i)
}

print(max(ds))
