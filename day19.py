from enum import Enum
import load_file

class Direction(Enum):
    up = 1
    right = 2
    down = 3
    left = 4


pipes = ['|', '-', '+']


def part1(diagram):
    pos = find_start(diagram)
    d = Direction.down
    letters = []
    while pos:
        pos, d = move(diagram, pos, d, letters)
    return letters

def part2(diagram):
    pos = find_start(diagram)
    d = Direction.down
    letters = []
    move_count = 0
    while pos:
        pos, d = move(diagram, pos, d, letters)
        move_count += 1
    return move_count


def find_start(diagram):
    for i, c in enumerate(diagram[0]):
        if c == '|':
            return (0, i)


def move(diagram, position, direction, letters):
    next_p = next_position(position, direction)
    next_direction = direction
    if not is_valid(diagram, next_p):
        for d in possible_directions(direction):
            temp_next_p = next_position(position, d)
            if is_valid(diagram, temp_next_p):
                next_p = temp_next_p
                next_direction = d
                break
        if direction == next_direction:
            return None, None
        
    if diagram[next_p[0]][next_p[1]] not in pipes:
        letters.append(diagram[next_p[0]][next_p[1]])
    return next_p, next_direction
    


def next_position(position, direction):
    if not position:
        return None
    next_p = None
    if direction == Direction.left:
        next_p = (position[0], position[1] - 1)
    if direction == Direction.right:
        next_p = (position[0], position[1] + 1)
    if direction == Direction.up:
        next_p = (position[0] - 1, position[1])
    if direction == Direction.down:
        next_p = (position[0] + 1, position[1])
    return next_p


def possible_directions(direction):
    if direction == Direction.left or direction == Direction.right:
        return [Direction.up, Direction.down]
    if direction == Direction.up or direction == Direction.down:
        return [Direction.left, Direction.right]


def is_valid(diagram, position):
    if not position:
        return True

    if position[0] < 0 or position[0] >= len(diagram):
        return False
    if position[1] < 0 or position[1] >= len(diagram[0]):
        return False

    cell = diagram[position[0]][position[1]]
    return cell is not ' '


def main():
    with load_file.open_file('day19.input.txt') as f:
        diagram = [list(x) for x in f]
        print(''.join(part1(diagram)))
        print(part2(diagram))


if __name__ == '__main__':
    main()
