import load_file

def word_to_dict(word):
    d = {}
    for letter in word:
        count = d.get(letter, 0)
        count += 1
        d[letter] = count
    return d

def is_valid(passphrase):
    length_map = {}
    for word in passphrase:
        word_list = length_map.get(len(word), [])
        length_map[len(word)] = word_list
        if word_list:
            word_list.append(word_to_dict(word))
            continue
        for d in word_list:
            d_local = d.copy()
            for letter in word:
                if letter not in d_local:
                    break
                count = d_local[letter]
                count = count - 1
                if count == 0:
                    del d_local[letter]
                else:
                    d_local[letter] = count
            if d_local:
                return False

        word_list.append(word_to_dict(word))
    return True

def main():
    valid = 0

    with load_file.open_file('day4.input.txt') as f:
        for line in f:
            if is_valid(line.split()):
                valid = valid + 1

    print(valid)

if __name__ == '__main__':
    main()
