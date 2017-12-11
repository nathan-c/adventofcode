import math
from enum import Enum

input = 312051
i = math.floor(input**0.5)
if i % 2 == 0:
    i -= 1

remainder = input - i**2

# now we can count round from bottom right corner

no_of_sides_traversed = remainder / (i + 1)
distance_round_side = remainder % (i + 1)
y = math.floor((i + 2) // 2)
x = distance_round_side - y

manhattan_distance = abs(x) + abs(y)

print(manhattan_distance)


class Direction(Enum):
    up = 1
    right = 2
    down = 3
    left = 4


board = {(0, 0): 1, (1, 0): 1}

direction = Direction.right
position = (1, 0)


def left(p):
    return (p[0] - 1, p[1])


def right(p):
    return (p[0] + 1, p[1])


def up(p):
    return (p[0], p[1] + 1)


def down(p):
    return (p[0], p[1] - 1)


def up_left(p):
    return up(left(p))


def up_right(p):
    return up(right(p))


def down_left(p):
    return down(left(p))


def down_right(p):
    return down(right(p))


def should_turn(d, p):
    if d == Direction.left:
        return down(p) not in board
    if d == Direction.right:
        return up(p) not in board
    if d == Direction.up:
        return left(p) not in board
    if d == Direction.down:
        return right(p) not in board


def turn(d):
    if d == Direction.left:
        return Direction.down
    if d == Direction.down:
        return Direction.right
    if d == Direction.right:
        return Direction.up
    if d == Direction.up:
        return Direction.left


def move(d, p):
    if d == Direction.left:
        return left(p)
    if d == Direction.down:
        return down(p)
    if d == Direction.right:
        return right(p)
    if d == Direction.up:
        return up(p)


def sum(p):
    return  board.get(up_left(p), 0) + board.get(up(p), 0) + board.get(up_right(p), 0) + board.get(right(p), 0) + board.get(down_right(p), 0) + board.get(down(p), 0) + board.get(down_left(p), 0) + board.get(left(p), 0)


while board[position] < input:
    if should_turn(direction, position):
        direction = turn(direction)
    position = move(direction, position)
    board[position] = sum(position)

print(board[position])