import sys, os

def is_valid(passphrase):
    length_map = {}
    for word in passphrase:
        l = length_map.get(len(word), [])
        length_map[len(word)] = l
        if len(l) == 0:
            l.append(word)
            continue
        
        for w in l:
            if w == word:
                return False
        l.append(word)
    return True

pathname = os.path.dirname(sys.argv[0])
inputfile = os.path.join(pathname, 'day4.input.txt')

valid = 0

with open(inputfile) as file:
    for line in file:
        if is_valid(line.split()):
            valid = valid + 1

print(valid)

