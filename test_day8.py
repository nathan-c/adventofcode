import day8

x = [
    'b inc 5 if a > 1/n',
    'a inc 1 if b < 5/n',
    'c dec -10 if a >= 1/n',
    'c inc -20 if c == 10/n'
]

def test_run():
    assert day8.run(x) == 1