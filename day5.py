import load_file

def run(instructions):
    step_count = 0
    i = 0
    while i >= 0 and i < len(instructions):
        instr = instructions[i]
        if instr > 2:
            instructions[i] -= 1
        else:
            instructions[i] += 1
        i += instr
        step_count += 1
    return step_count

def main():
    with load_file.open_file('day5.input.txt') as f:
        print(run([int(x) for x in f]))

if __name__ == '__main__':
    main()
    