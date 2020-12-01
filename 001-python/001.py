import sys

with open(sys.argv[1]) as f:
    data = sorted(map(int, f.read().splitlines()))

sdata = set(data)
llen = len(data)

# a
for x in data:
    target = 2020 - x
    if target in sdata:
        print(x * target)
        break

# b
for i in range(llen):
    y = 2020 - data[i]
    for j in range(llen - 1, i, -1):
        target = y - data[j]
        if target in sdata:
            print(data[i] * data[j] * target)
            raise SystemExit(0)
