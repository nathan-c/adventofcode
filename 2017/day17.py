
def run_one(buf, iteration, i, x):
    i = (x + i) % len(buf)
    buf = buf[:i + 1] + [iteration] + buf[i + 1:]
    i = i + 1
    return i, buf

def run_part2():
    item_after_0 = 0
    i = 0
    for iteration in range(1, 50000001):
        i = (371 + i) % iteration
        if i == 0:
            item_after_0 = iteration
        i += 1
    return item_after_0


def run_part1():
    buf = [0]
    index = 0
    for iteration in range(1, 2018):
        index, buf = run_one(buf, iteration, index, 371)
    #print(buf)
    return_next = False
    for x in buf:
        if return_next:
            return x
        if x == 0:
            return_next = True


print(run_part1())
print(run_part2())
