import day14

def test_run():
    assert day14.run("flqrgnkx") == 8108

def test_formatting():
    assert format(1,'04b') == '0001'
    assert format(0xf,'04b') == '1111'