import day10


def run_part1(key):
    ''' This is really slow :( '''
    sum_board = 0
    for i in range(128):
        key_i = '{}-{}'.format(key, i)
        # run hash
        data = day10.knot_hash(key_i)
        # get hex output
        hex_data = day10.format_output(data, '02x')
        # convert to binary then sum all digits
        count = sum(sum(sum(int(j) for j in b)
                        for b in format(int(x, 16), '04b')) for x in hex_data)
        sum_board += count
    return sum_board


def run_part2(key):
    ''' This is really slow :( '''
    num_regions = 0
    # previous_row = ''.join(0 for x in range(128))
    board = []
    for i in range(128):
        key_i = '{}-{}'.format(key, i)
        # run hash
        data = day10.knot_hash(key_i)
        # get hex output
        hex_data = day10.format_output(data, '02x')
        # convert to binary then sum all digits
        row = ''.join(format(int(x, 16), '04b') for x in hex_data)
        board.append(list(row))
    for r in range(len(board)):
        for c in range(len(board[0])):
            num_regions += fill_region(board, r, c)
    return num_regions


def fill_region(board, row, col):
    if board[row][col] != '1': return 0
    q=[(row, col)]
    while q:
        r, c=q.pop()
        for child in get_joining(board, r, c):
            q.append(child)
        board[r][c]='0'
    return 1



def get_joining(board, row, col):
    cl=row, col - 1
    bc=row + 1, col
    cr=row, col + 1
    tc=row - 1, col
    possible_vals=[cl, bc, cr, tc]
    height=len(board)
    width=len(board[0])
    for r, c in possible_vals:
        if r >= 0 and r < height and c >= 0 and c < width and board[r][c] == '1':
            yield r, c


def main():
    print(run_part1('hxtvlmkl'))
    print(run_part2('hxtvlmkl'))


if __name__ == '__main__':
    main()
