import day12

graph = {
    0: [2],
    1: [1],
    2: [0, 3, 4],
    3: [2, 4],
    4: [2, 3, 6],
    5: [6],
    6: [4, 5]
}


def test_size_of_group():
    assert day12.size_of_group(graph, 0) == 6


def test_parse_input():
    input_data = [
        '0 <-> 2',
        '1 <-> 1',
        '2 <-> 0, 3, 4',
        '3 <-> 2, 4',
        '4 <-> 2, 3, 6',
        '5 <-> 6',
        '6 <-> 4, 5',
    ]
    assert day12.parse_input(input_data) == graph


def test_number_of_groups():
    assert day12.number_of_groups(graph) == 2
