

def max_index(x):
    max_no = x[0]
    max_i = 0
    for i, a in enumerate(x):
        if a > max_no:
            max_i = i
            max_no = a
    return (max_i, max_no)

def run(banks):
    no = len(banks)
    i = 0
    seen = {}
    while banks not in seen:
        max(banks)