import day6

def test_run():
    assert day6.run([0, 2, 7, 0]) == 4

def test_max_index():
    assert day6.max_index((1, 2, 3, 4)) == (3, 4)

def test_array_equality():
    assert [1, 2, 3] == [1, 2, 3]

def test_set():
    s = set()
    l = (1,2,3)
    assert l not in s