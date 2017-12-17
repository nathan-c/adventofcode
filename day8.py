import load_file

operations = {
    '<': lambda register, operand: register < operand,
    '>': lambda register, operand: register > operand,
    '>=': lambda register, operand: register >= operand,
    '<=': lambda register, operand: register <= operand,
    '==': lambda register, operand: register == operand,
    '!=': lambda register, operand: register != operand,
}

def parse_instruction(instruction):
    x = instruction.split()
    predicate = lambda register: operations[x[5]](register, int(x[6]))
    return x[0], int(x[2]) if x[1] == 'inc' else -int(x[2]), x[4], predicate

def run(instructions):
    running_max = 0
    registers = {}
    for instruction in instructions:
        reg, size, pred_reg, pred = parse_instruction(instruction)
        pred_reg = registers.get(pred_reg, 0)
        if pred(pred_reg):
            new_val = registers.get(reg, 0) + size
            running_max = max(new_val, running_max)
            registers[reg] = new_val
    return max(registers.values()), running_max


def main():
    with load_file.open_file('day8.input.txt') as f:
        print(run(f))

if __name__ == '__main__':
    main()