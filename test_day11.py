import day11


def test_run():
    assert day11.run(['ne', 'ne', 'ne']) == 3
    assert day11.run(['ne', 'ne', 'sw', 'sw']) == 0
    assert day11.run(['ne', 'ne', 's', 's']) == 2
    assert day11.run(['se', 'sw', 'se', 'sw', 'sw']) == 3
