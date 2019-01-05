import day18


def test_p1():
    t = ['set a 1', 'add a 2', 'mul a a',  'mod a 5', 'snd a',
         'set a 0', 'rcv a',   'jgz a -1', 'set a 1', 'jgz a -2']
    assert day18.part1(t) == 4

def test_p2():
    t = ['snd 1','snd 2','snd p','rcv a','rcv b','rcv c','rcv d']
    assert day18.part2(t) == 3