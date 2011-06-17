ds d1

rec = [key = 5, val = 1337]
rec = [key = 10, val = 1337]

for(i = 0; i < 20; i++)
{
	insert(d1, rec)
	update(d1, rec, rec2)
}

print(max(d1))
