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


def run_part1(key):
    ''' This is really slow :( '''
    num_regions = 0
    #previous_row = ''.join(0 for x in range(128))
    board = []
    for i in range(128):
        key_i = '{}-{}'.format(key, i)
        # run hash
        data = day10.knot_hash(key_i)
        # get hex output
        hex_data = day10.format_output(data, '02x')
        # convert to binary then sum all digits
        row = ''.join(b for b in format(int(x, 16), '04b')) for x in hex_data)
        board.append(row)
    for row in board:
        for col in row:
            if board[row][col] == '1':
                fill_region(board, row, col)
                num_regions += 1
    return sum_board


def fill_region(board, row, col):



def get_joining(board, row, col):
    tl = row - 1, col - 1
    cl = row, col - 1
    bl = row + 1, col - 1
    bc = row + 1, col
    br = row + 1, col + 1
    cr = row, col + 1
    tr = row - 1, col + 1
    tc = row - 1, col
    possible_vals = [tl, cl, bl, bc, br, cr, tr, tc]
    height = len(board)
    width = len(board[0])
    for r, c in possible_vals:
        if r > 0 and r < height and c> 0 and r < width:
            yield r, c


def main():
    print(run('hxtvlmkl'))


if __name__ == '__main__':
    main()
