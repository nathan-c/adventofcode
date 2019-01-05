import day14

def test_run_part1():
    assert day14.run_part1("flqrgnkx") == 8108

def test_run_part2():
    assert day14.run_part2("flqrgnkx") == 1242

def test_formatting():
    assert format(1,'04b') == '0001'
    assert format(0xf,'04b') == '1111'
