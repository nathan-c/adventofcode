operations = {
    '<': lambda register, operand: register < operand,
    '>': lambda register, operand: register > operand,
    '>=': lambda register, operand: register >= operand,
    '<=': lambda register, operand: register <= operand,
    '==': lambda register, operand: register == operand,
    '!=': lambda register, operand: register != operand,
}

def parse_line(line):
    x = line.split()
    predicate = lambda register: operations[x[5]](register, x[6])
    return x[0], x[2] if x[1] == 'inc' else -x[2], x[4], predicate


def get_register(registers, register):
    ret_val = registers.get(register, None)
    if ret_val:
        return ret_val
    registers[register] = 0
    return 0

