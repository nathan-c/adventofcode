import load_file
import statistics

class TreeNode():
    def __init__(self, value, weight, parent):
        self.parent = parent
        self.value = value
        self.cumulative_weight = weight
        self.weight = weight
        self.children = []
    def add_child(self, child, weight):
        self.children.append(TreeNode(child, weight, self))
        self.increase_weight(weight)
    def increase_weight(self, new_weight):
        if self.parent:
            self.parent.increase_weight(new_weight)
        self.cumulative_weight += new_weight
    def get_siblings(self):
        if self.parent:
            for sibling in self.parent.children:
                if sibling != self:
                    yield sibling

def build_tree(tree_dict, weights):
    root_value = find_root(tree_dict)
    root = TreeNode(root_value, weights[root_value], None)
    add_children(root, tree_dict, weights)
    return root
    
def add_children(tree_node, tree_dict, weights):
    for c in tree_dict.get(tree_node.value):
        tree_node.add_child(c, weights[c])
    for c in tree_node.children:
        add_children(c, tree_dict, weights)

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
    if current_word != '':
        split_line.append(current_word)
    return (split_line[0], split_line[1], split_line[2:])

def process_file(filename):
    tree = {}
    weights = {}
    with load_file.open_file(filename) as f:
        for line in f:
            name, weight, children = parse_line(line)
            tree[name] = children
            weights[name] = int(weight)
    
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

def find_odd_weight(tree_node):
    bad1, bad2 = find_bad_branch(tree_node)
    if not bad1 and not bad2:
        return tree_node
    if not bad2:
        return find_odd_weight(bad1)
    test1 = find_odd_weight(bad1)
    test2 = find_odd_weight(bad2)
    if test1 == bad1 and test2 == bad2:
        # we know the problem exists at this level return either
        return bad1
    if test1:
        return test2
    else:
        return test1


def find_bad_branch(tree_node):
    lower = []
    upper = []
    mean = statistics.mean([x.cumulative_weight for x in tree_node.children])
    for child in tree_node.children:
        if mean == child.cumulative_weight:
            return None, None
        if child.cumulative_weight < mean:
            lower.append(child)
        else:
            upper.append(child)
    if len(lower) == 1 and len(upper) == 1:
        return lower[0], upper[0]
    return lower[0] if len(lower) == 1 else upper[0], None

def calculate_correct_weight(bad_node):
    for sibling in bad_node.get_siblings():
        return sibling.cumulative_weight - bad_node.cumulative_weight + bad_node.weight

def main(filename):
    tree_dict, weights = process_file(filename)
    tree = build_tree(tree_dict, weights)
    node = find_odd_weight(tree)    
    corrext_weight = calculate_correct_weight(node)
    return corrext_weight

if __name__ == '__main__':
    print(main('day7.input.txt'))