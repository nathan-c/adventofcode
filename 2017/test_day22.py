import day22


def run_test(no_of_runs, infected, expected_value):
    current_node = (0, 0)
    direction = day22.Direction.up
    infections_caused = 0
    for _ in range(no_of_runs):
        current_node, direction, caused_infection = day22.burst_part1(
            infected, current_node, direction)
        if caused_infection:
            infections_caused += 1
    assert infections_caused == expected_value


def test_day22_7():
    infected = set([(-1, 0), (1, 1)])
    assert day22.run_part1(7, infected) == 5


def test_day22_70():
    infected = set([(-1, 0), (1, 1)])
    assert day22.run_part1(70, infected) == 41


def test_day22_10000():
    infected = set([(-1, 0), (1, 1)])
    assert day22.run_part1(10000, infected) == 5587


def test_parse_input():
    test_input = [
        "..#",
        "#..",
        "..."
    ]
    assert day22.parse_input(test_input) == set([(-1, 0), (1, 1)])


def test_run_part2_100():
    infected = set([(-1, 0), (1, 1)])
    assert day22.run_part2(100, infected) == 26


def test_run_part2_10000000():
    infected = set([(-1, 0), (1, 1)])
    assert day22.run_part2(10000000, infected) == 2511944
