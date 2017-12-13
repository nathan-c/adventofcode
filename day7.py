import load_file

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

# def find_odd_weight(tree_node):
#     q = [tree_node]
#     while q:
#         current_node = q.pop()
#         for child in current_node.children:
#             if child.cumulative_weight - child.weight == 0:
                
#             q.append(child)
        

#     d = {}
#     for child in tree_node.children:
#         count, nodes = d.get(child.cumulative_weight, (0, []))
#         nodes.append(child)
#         d[child.cumulative_weight] = (count + 1, nodes)
#     lonely_weights = [x for x in d.items() if x[1][0] == 1]
#     if len(lonely_weights) == 1:
#         # we have found the weight that is different or there is only 1 child
#         return find_odd_weight(lonely_weights[0])
#     elif len(lonely_weights) == 2:
#         # only 2 children so both weights only appear once
#     elif:
#         # all are the 


def main():
    tree, weights = process_file('day7.input.txt')
    root = find_root(tree)
    print(root)

if __name__ == '__main__':
    main()