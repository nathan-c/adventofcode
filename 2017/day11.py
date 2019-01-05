import load_file


def manhattan_distance(*distances):
    return sum(map(abs, distances))


def run(path):
    max_manhattan = 0
    x, y, z = 0, 0, 0
    direction_map = {
        'ne': lambda x, y, z: (x + 0.5, y + 0.5, z),
        'sw': lambda x, y, z: (x - 0.5, y - 0.5, z),
        'n': lambda x, y, z: (x, y + 0.5, z + 0.5),
        's': lambda x, y, z: (x, y - 0.5, z - 0.5),
        'nw': lambda x, y, z: (x, y - 0.5, z + 0.5),
        'se': lambda x, y, z: (x, y + 0.5, z - 0.5),
    }
    for direction in path:
        x, y, z = direction_map[direction](x, y, z)
        max_manhattan = max(manhattan_distance(x, y, z), max_manhattan)
    return manhattan_distance(x, y, z), max_manhattan


def main():
    def strip(s): return s.strip()
    with load_file.open_file('day11.input.txt') as f:
        for l in f:
            print(run(map(strip, l.split(','))))


if __name__ == '__main__':
    main()
