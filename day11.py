import load_file


def manhattan_distance(*distances):
    return sum(map(abs, distances))


def run(path):
    max_manhattan = 0
    x, y, z = 0, 0, 0
    direction_map = {
        'ne': lambda x, y, z: (x + 1, y + 1, z),
        'sw': lambda x, y, z: (x - 1, y - 1, z),
        'n': lambda x, y, z: (x, y + 1, z + 1),
        's': lambda x, y, z: (x, y - 1, z - 1),
        'nw': lambda x, y, z: (x, y - 1, z + 1),
        'se': lambda x, y, z: (x, y + 1, z - 1),
    }
    for direction in path:
        x, y, z = direction_map[direction](x, y, z)
        max_manhattan = max(manhattan_distance(x, y, z) // 2, max_manhattan)
    return manhattan_distance(x, y, z) // 2, max_manhattan


def main():
    def strip(s): return s.strip()
    with load_file.open_file('day11.input.txt') as f:
        for l in f:
            print(run(map(strip, l.split(','))))


if __name__ == '__main__':
    main()
