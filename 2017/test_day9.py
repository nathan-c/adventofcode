import day9

def test_1():
    assert day9.run('{}')[0] == 1

def test_2():
    assert day9.run('{{{}}}')[0] == 6

def test_3():
    assert day9.run('{{},{}}')[0] == 5

def test_4():
    assert day9.run('{{{},{},{{}}}}')[0] == 16

def test_5():
    assert day9.run('{<a>,<a>,<a>,<a>}')[0] == 1

def test_6():
    assert day9.run('{{<ab>},{<ab>},{<ab>},{<ab>}}')[0] == 9

def test_7():
    assert day9.run('{{<!!>},{<!!>},{<!!>},{<!!>}}')[0] == 9

def test_8():
    assert day9.run('{{<a!>},{<a!>},{<a!>},{<ab>}}')[0] == 3
