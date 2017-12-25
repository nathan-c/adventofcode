import day10

def run(key):
    board = []
    for i in range(128):
        lengths = day10.get_lengths('{}-{}'.format(key, i))
        _, _, data = day10.single_round(lengths, 0, 0, [range(128)])
        data = day10.format_output(day10.compress_hash(data), '04b')
        board.append(data)
    return sum(1 for x in board)