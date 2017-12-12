import day6

def test_run():
    assert day6.run([0, 2, 7, 0]) == 5

def test_max_index():
    assert day6.max_index((1,2,3,4)) == (3, 4)