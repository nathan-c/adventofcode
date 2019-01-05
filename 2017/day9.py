import load_file

class TreeNode():
    def __init__(self, parent):
        self.parent = parent
        self.children = []
    def add_child(self):
        child = TreeNode(self)
        self.children.append(child)
        return child

def run(stream):
    garbage = False
    root = TreeNode(None)
    current = root
    skip = False
    sum_garbage = 0
    for char in stream:
        if not skip and char == '!':
            skip = True
        elif not garbage and char == '{' and not skip:
            current = current.add_child()
        elif not garbage and char == '<' and not skip:
            garbage = True
        elif garbage and char == '>' and not skip:
            garbage = False
        elif not garbage and char == '}' and not skip:
            current = current.parent
        elif skip:
            skip = False
        elif garbage and not skip:
            sum_garbage += 1
    score = add_scores(root)
    return score, sum_garbage

def add_scores(tree, depth=0):
    score = depth
    for child in tree.children:
        score += add_scores(child, depth + 1)

    return score

def main():
    with load_file.open_file('day9.input.txt') as f:
        for stream in f:
            print(run(stream))

if __name__ == '__main__':
    main()
