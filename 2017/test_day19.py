import day19


def test_part1():
    diagram = ['     |          ',
               '     |  +--+    ',
               '     A  |  C    ',
               ' F---|----E|--+ ',
               '     |  |  |  D ',
               '     +B-+  +--+ ']
    assert ''.join(day19.part1([list(x) for x in diagram])) == 'ABCDEF'

def test_part2():
    diagram = ['     |          ',
               '     |  +--+    ',
               '     A  |  C    ',
               ' F---|----E|--+ ',
               '     |  |  |  D ',
               '     +B-+  +--+ ']
    assert day19.part2([list(x) for x in diagram]) == 38
