def max_index(x):
    max_no = x[0]
    max_i = 0
    for i, a in enumerate(x):
        if a > max_no:
            max_i = i
            max_no = a
    return (max_i, max_no)

def run(banks):
    no_cycles = 0
    no_banks = len(banks)
    seen = set()
    while tuple(banks) not in seen:
        seen.add(tuple(banks))
        i, no = max_index(banks)
        banks[i] = 0
        for count in range(no):
            current_i = (i + count + 1) % no_banks
            banks[current_i] = banks[current_i] + 1
        no_cycles += 1
    return no_cycles


def main():
    print(run([int(x) for x in '4	1	15	12	0	9	9	5	5	8	7	3	14	5	12	3'.split()]))

if __name__ == '__main__':
    main()
