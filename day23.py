import load_file


class Duet(object):
    def __init__(self):
        self._registers = {}
        self._freq = 0

    def set(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = int_y

    def sub(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = self._registers.get(x, 0) - int_y

    def mul(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = self._registers.get(x, 0) * int_y

    def jnz(self, x, y):
        int_x = self.get_int(x)
        int_y = self.get_int(y)
        if int_x != 0:
            return int_y
        return None

    def get_int(self, x):
        try:
            return int(x)
        except ValueError:
            return self._registers.get(x, 0)


inst_map = {
    'set': lambda d, r: d.set(r[1], r[2]),
    'sub': lambda d, r: d.sub(r[1], r[2]),
    'mul': lambda d, r: d.mul(r[1], r[2]),
    'jnz': lambda d, r: d.jnz(r[1], r[2]),
}


def run1(instructions):
    duet = Duet()
    i = 0
    mul_count = 0
    while i >= 0 and i < len(instructions):
        a = instructions[i]
        val = inst_map[a[0]](duet, a)

        if a[0] == 'mul':
            mul_count += 1

        if a[0] == 'jnz' and val:
            i += val
        else:
            i += 1
    return mul_count


def run2():
    h = 0
    # After running thru the first 10 iterations we see that b is set to 105700
    # c is set to 122700 and never changed
    # the program will exit when b==c (at line 29)
    # b is only ever incremented by 17 towards the end of the instructions.
    # For it to hit this instruction g!=0 at line 29
    # h only ever increments by 1 is f==0 at line 25
    # this only ever happens if g==0 at line 15
    # g==0 at line 15 if d*e==b
    # If g has any prime factors then h will be incremented
    for x in xrange(105700, 122700 + 1, 17):
        for i in xrange(2, x):
            if x % i == 0:
                h += 1
                break
    return h


def part1(instructions):
    instructions = [x.strip().split(' ') for x in instructions]
    return run1(instructions)


def part2(instructions):
    instructions = [x.strip().split(' ') for x in instructions]
    return run2()


def main():
    with load_file.open_file('day23.input.txt') as f:
        # print(part1(f))
        print(part2(f))


if __name__ == '__main__':
    main()
