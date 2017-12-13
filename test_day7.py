import day7

def test_line():
    x = day7.parse_line('fwft (72) -> ktlj, cntj, xhth')
    assert len(x) == 3
    assert len(x[2]) == 3
    assert x[0] == 'fwft'
    assert x[1] == '72'
    assert x[2][0] == 'ktlj'
    assert x[2][1] == 'cntj'
    assert x[2][2] == 'xhth'

def test_process_file():
    tree, weights = day7.process_file('test_day7.input.txt')
    assert tree


def test_find_root():
    tree, weights = day7.process_file('test_day7.input.txt')
    root = day7.find_root(tree)
    assert root == 'tknk'