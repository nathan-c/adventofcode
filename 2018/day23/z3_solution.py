from z3 import If, Int, Optimize
import re

# I found this on on reddit and it intrigued me.
# quite a nice way to solve the problem as a series of equations defining the boundaries of x,y,z

rx = r'pos=<([-\d]+),([-\d]+),([-\d]+)>, r=([-\d]+)'

nanobots = []
with open('input.txt') as f:
    for l in f:
        bot = (int(x) for x in re.findall(rx, l)[0])
        nanobots.append(bot)


def zabs(x):
    return If(x >= 0, x, -x)

x = Int('x')
y = Int('y')
z = Int('z')

in_ranges = [Int('in_range_' + str(i)) for i in range(len(nanobots))]

range_count = Int('sum')

o = Optimize()

for i in range(len(nanobots)):
    nx, ny, nz, nrng = nanobots[i]
    o.add(in_ranges[i] == If(zabs(x - nx) +
                             zabs(y - ny) + zabs(z - nz) <= nrng, 1, 0))

o.add(range_count == sum(in_ranges))

dist_from_zero = Int('dist')

o.add(dist_from_zero == zabs(x) + zabs(y) + zabs(z))

# optimise the solution for maximum range count value (i.e. most bots in range of x,y,z point)
h1 = o.maximize(range_count)
# then optimise the solution for minimum distance to the origin
h2 = o.minimize(dist_from_zero)

print(o.check())
print(o.lower(h1))
print(o.upper(h1))
print("b", o.lower(h2), o.upper(h2))  # 121167568
print(o.model()[x])
print(o.model()[y])
print(o.model()[z])
