from enum import IntEnum
import load_file


class Direction(IntEnum):
    up = 0
    right = 1
    down = 2
    left = 3


direction_map = {
    Direction.up: (0, 1),
    Direction.down: (0, -1),
    Direction.left: (-1, 0),
    Direction.right: (1, 0)
}


def burst_part1(infected, current_node, direction):
    caused_infection = False
    if current_node in infected:
        direction = turn_right(direction)
        infected.remove(current_node)
    else:
        direction = turn_left(direction)
        infected.add(current_node)
        caused_infection = True
    move = direction_map[direction]
    return (current_node[0] + move[0], current_node[1] + move[1]), direction, caused_infection


def burst_part2(infected, weakened, flagged, current_node, direction):
    caused_infection = False
    if current_node in infected:
        direction = turn_right(direction)
        flagged.add(current_node)
        infected.remove(current_node)
    elif current_node in weakened:
        infected.add(current_node)
        weakened.remove(current_node)
        caused_infection = True
    elif current_node in flagged:
        direction = reverse_direction(direction)
        flagged.remove(current_node)
    else:
        direction = turn_left(direction)
        weakened.add(current_node)
    move = direction_map[direction]
    return (current_node[0] + move[0], current_node[1] + move[1]), direction, caused_infection


def reverse_direction(direction):
    return Direction((direction + 2) % len(Direction))


def turn_right(direction):
    return Direction((direction + 1) % len(Direction))


def turn_left(direction):
    return Direction((direction - 1) % len(Direction))


def run_part1(no_of_runs, infected):
    current_node = (0, 0)
    direction = Direction.up
    infections_caused = 0
    for _ in xrange(no_of_runs):
        current_node, direction, caused_infection = burst_part1(
            infected, current_node, direction)
        if caused_infection:
            infections_caused += 1
    return infections_caused


def run_part2(no_of_runs, infected):
    current_node = (0, 0)
    direction = Direction.up
    infections_caused = 0
    weakened = set()
    flagged = set()
    for _ in xrange(no_of_runs):
        current_node, direction, caused_infection = burst_part2(
            infected, weakened, flagged, current_node, direction)
        if caused_infection:
            infections_caused += 1
    return infections_caused


def parse_input(file):
    row_count = len(file)
    col_count = len(file[0])
    infected = set()
    for row_num, row in enumerate(file):
        for col_num, item in enumerate(row):
            if item == '#':
                x = col_num - ((col_count - 1) // 2)
                y = ((row_count - 1) // 2) - row_num
                infected.add((x, y))
    return infected


def main():
    with load_file.open_file('day22.input.txt') as f:
        file_data = [row for row in f]
        infected = parse_input(file_data)
        print(run_part1(10000, infected))
        infected = parse_input(file_data)
        print(run_part2(10000000, infected))


if __name__ == '__main__':
    main()
