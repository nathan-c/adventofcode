import day10


def test_run():
    assert day10.run([3, 4, 1, 5], 5) == 12


def test_get_lengths():
    assert day10.get_lengths('1,2,3') == [
        49, 44, 50, 44, 51, 17, 31, 73, 47, 23]


def test_compress_hash():
    assert day10.compress_hash(
        [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22]) == [64]


def test_hex():
    assert format(1, '02x') == '01'
    assert format(255, '02x') == 'ff'

def test_knot_hash():
    assert day10.format_output(day10.knot_hash('')) == 'a2582a3a0e66e6e86e3812dcb672a272'
    assert day10.format_output(day10.knot_hash('AoC 2017')) == '33efeb34ea91902bb2f59c9920caa6cd'
    assert day10.format_output(day10.knot_hash('1,2,3')) == '3efbe78a8d82f29979031a4aa0b16a9d'
    assert day10.format_output(day10.knot_hash('1,2,4')) == '63960835bcdc130f0b66d7ff4f6a5a8e'