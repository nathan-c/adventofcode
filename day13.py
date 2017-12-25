import load_file
import itertools


def attempt_firewall(fire_wall):
    severity = 0
    max_depth = max(fire_wall.keys())
    for i in range(max_depth + 1):
        if i in fire_wall and i % (fire_wall[i] * 2 - 2) == 0:
            severity += i * fire_wall[i]
    return severity


def try_attempt_firewall(fire_wall):
    pause_length = 0
    while not _try_attempt_firewall(fire_wall, pause_length):
        pause_length += 1
    return pause_length


def _try_attempt_firewall(fire_wall, pause_length):
    max_depth = max(fire_wall.keys())
    for i in range(max_depth + 1):
        if i in fire_wall and (i + pause_length) % (fire_wall[i] * 2 - 2) == 0:
            return False
    return True


def try_attempt_firewall_fast2(fire_wall):
    common_factors = set([k + v for k, v in fire_wall.items()])
    lowest_common_multiple = 1
    for x in common_factors:
        lowest_common_multiple *= x
    return lowest_common_multiple - 1


def try_attempt_firewall_fast(fire_wall):
    def scanner(height, time):
        offset = time % ((height - 1) * 2)
        return 2 * (height - 1) - offset if offset > height - 1 else offset

    for i in itertools.count():
        for position in fire_wall:
            if not any(scanner(fire_wall[position], position + i)) == 0:
                return i


def parse_firewall(input_data):
    fire_wall={}
    for line in input_data:
        split_line=line.split(':')
        fire_wall[int(split_line[0].strip())]=int(split_line[1].strip())
    return fire_wall


def main():
    with load_file.open_file('day13.input.txt') as f:
        fire_wall=parse_firewall(f)
        print(attempt_firewall(fire_wall))
        i=0
        print(try_attempt_firewall_fast(fire_wall))


if __name__ == '__main__':
    main()
