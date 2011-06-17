ds = DS

rec = [key = 5, val = 1337]
rec = [key = 10, val = 1337]

for(i = 0; i < 20; i++)
{
	insert(ds, rec)
	update(ds, rec, rec2)
}

print(max(ds))
