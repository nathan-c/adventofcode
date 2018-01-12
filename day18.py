import load_file
import threading
import queue


class Duet():
    def __init__(self):
        self._registers = {}
        self._freq = 0

    def snd(self, x):
        int_x = self.get_int(x)
        self._freq = int_x

    def rcv(self, x):
        int_x = self.get_int(x)
        if int_x != 0:
            return self._freq

    def set(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = int_y
        #print('{}-set {} {} -> {}'.format(threading.get_ident(), x, y, int_y))

    def add(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = self._registers.get(x, 0) + int_y
        # print('{}-add {} {} -> {}'.format(threading.get_ident(),
        #                                   x, y, self._registers[x]))

    def mul(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = self._registers.get(x, 0) * int_y
        # print('{}-mul {} {} -> {}'.format(threading.get_ident(),
        #                                   x, y, self._registers[x]))

    def mod(self, x, y):
        int_y = self.get_int(y)
        self._registers[x] = self._registers.get(x, 0) % int_y
        # print('{}-mod {} {} -> {}'.format(threading.get_ident(),
        #                                   x, y, self._registers[x]))

    def jgz(self, x, y):
        int_x = self.get_int(x)
        int_y = self.get_int(y)
        # print('{}-jgz {} {} -> {}'.format(threading.get_ident(), x, y, int_y))
        if int_x > 0:
            return int_y

    def snd2(self, x, q):
        #print('{}-snd2 {}'.format(threading.current_thread().name, x))
        self._freq += 1
        int_x = self.get_int(x)
        q.put_nowait(int_x)
        # if(threading.current_thread().name == 't1'):
        #     print(self._freq)

    def rcv2(self, x, q):
        #print('{}-rcv2 {}'.format(threading.current_thread().name, x))
        y = q.get(timeout=2)
        self._registers[x] = y

    def get_int(self, x):
        try:
            return int(x)
        except ValueError:
            return self._registers.get(x, 0)


inst_map = {
    'snd': lambda d, r: d.snd(r[1]),
    'set': lambda d, r: d.set(r[1], r[2]),
    'add': lambda d, r: d.add(r[1], r[2]),
    'mul': lambda d, r: d.mul(r[1], r[2]),
    'mod': lambda d, r: d.mod(r[1], r[2]),
    'rcv': lambda d, r: d.rcv(r[1]),
    'jgz': lambda d, r: d.jgz(r[1], r[2]),
}
inst_map2 = {
    'snd': lambda d, r, qsnd, qrcv: d.snd2(r[1], qsnd),
    'set': lambda d, r, qsnd, qrcv: d.set(r[1], r[2]),
    'add': lambda d, r, qsnd, qrcv: d.add(r[1], r[2]),
    'mul': lambda d, r, qsnd, qrcv: d.mul(r[1], r[2]),
    'mod': lambda d, r, qsnd, qrcv: d.mod(r[1], r[2]),
    'rcv': lambda d, r, qsnd, qrcv: d.rcv2(r[1], qrcv),
    'jgz': lambda d, r, qsnd, qrcv: d.jgz(r[1], r[2]),
}


def run1(instructions):
    duet = Duet()
    i = 0
    while i >= 0 or i < len(instructions):
        a = instructions[i]
        val = inst_map[a[0]](duet, a)

        if a[0] == 'rcv' and val:
            return val
        if a[0] == 'jgz' and val:
            i += val
        else:
            i += 1


def run2(duet, instructions, qsnd, qrcv):
    i = 0
    while i >= 0 and i < len(instructions):
        a = instructions[i]
        val = inst_map2[a[0]](duet, a, qsnd, qrcv)

        if a[0] == 'jgz' and val:
            i += val
        else:
            i += 1


def part1(instructions):
    instructions = [x.strip().split(' ') for x in instructions]
    return run1(instructions)


def part2(instructions):
    instructions = [x.strip().split(' ') for x in instructions]
    q1 = queue.Queue()
    q2 = queue.Queue()
    d0 = Duet()
    d0.set('p', 0)
    d1 = Duet()
    d1.set('p', 1)
    t0 = threading.Thread(target=run2, args=(d0, instructions, q1, q2), name='t0')
    t1 = threading.Thread(target=run2, args=(d1, instructions, q2, q1), name='t1')
    try:
        t0.start()
        t1.start()
        t0.join()
        t1.join()
    except queue.Empty:
        pass
    return d1._freq


def main():
    with load_file.open_file('day18.input.txt') as f:
        # print(part1(f))
        print(part2(f))


if __name__ == '__main__':
    main()
