import day24


def test_build_graph():
    parts = [
        (0, 2, 0),
        (2, 2, 1),
        (2, 3, 2),
        (3, 4, 3),
        (3, 5, 4),
        (0, 1, 5),
        (10, 1, 6),
        (9, 10, 7)
    ]
    assert day24.build_graph(parts)


def test_get_strongest_bridge():
    parts = [
        (0, 2, 0),
        (2, 2, 1),
        (2, 3, 2),
        (3, 4, 3),
        (3, 5, 4),
        (0, 1, 5),
        (10, 1, 6),
        (9, 10, 7)
    ]
    root = day24.build_graph(parts)
    assert day24.get_strongest_bridge(root) == 31
