import day10


def run(key):
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


def main():
    print(run('hxtvlmkl'))


if __name__ == '__main__':
    main()
