with open("001.txt") as f:
    data = list(map(int, f.read().splitlines()))

sdata = set(data)
slen = len(data)

# a
for x in data:
    target = 2020 - x
    if target in sdata:
        print(x * target)
        break

# b
for i in range(slen):
    y = 2020 - data[i]
    for j in range(i, slen):
        target = y - data[j]
        if target in sdata:
            print(data[i] * data[j] * target)
            raise SystemExit(0)
