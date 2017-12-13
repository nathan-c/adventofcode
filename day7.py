import load_file

def parse_line(line):
    split_line = []
    current_word = ''
    s = set(['(', ')', ',', ' ', '-', '>', '\n'])
    for c in line:
        if c not in s:
            current_word += c
        elif current_word:
            split_line.append(current_word)
            current_word = ''
    return (split_line[0], split_line[1], split_line[2:])

def process_file(filename):
    tree = {}
    weights = {}
    with load_file.open_file(filename) as f:
        for line in f:
            name, weight, children = parse_line(line)
            tree[name] = children
            weights[name] = weight
    
    return tree, weights

def find_root(tree_dict):
    all_names = []
    all_children = set()
    for k, v in tree_dict.items():
        for child in v:
            all_children.add(child)
        all_names.append(k)
    for name in all_names:
        if name not in all_children:
            return name

def main():
    tree, weights = process_file('day7.input.txt')
    root = find_root(tree)
    print(root)

if __name__ == '__main__':
    main()