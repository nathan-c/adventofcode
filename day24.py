import load_file


class GraphNode(object):
    def __init__(self, part, next_port):
        self.parent = None
        self.children = []
        self.part = part
        self.weight = part[0] + part[1]
        self.next_port = next_port
        self.path = set([part])

    def add_child(self, child):
        if child.parent:
            raise ValueError('New child already has a parent!')
        self.children.append(child)
        child.parent = self
        child.weight += self.weight
        child.path |= self.path

    def can_connect(self, part):
        return True if part not in self.path and (part[0] == self.part[self.next_port] or part[1] == self.part[self.next_port]) else False


def build_graph(parts):
    root = GraphNode((0, 0), 0)
    _build_graph(parts, root)
    return root


def get_strongest_bridge(node):
    strongest = node.weight
    for child in node.children:
        new_strongest = get_strongest_bridge(child)
        if new_strongest > strongest:
            strongest = new_strongest
    return strongest


def get_longest_bridge(node, current_height=0):
    weight = node.weight
    height = current_height
    for child in node.children:
        new_weight, new_height = get_longest_bridge(child, current_height + 1)
        if new_height == height and new_weight > weight:
            weight = new_weight
        if new_height > height:
            weight = new_weight
            height = new_height

    return weight, height


def _build_graph(parts, graph):
    for i, part in enumerate(parts):
        if graph.can_connect(part):
            next_port = 1 if part[0] == graph.part[graph.next_port] else 0
            new_node = GraphNode(part, next_port)
            graph.add_child(new_node)
            _build_graph(parts, new_node)


def run_part1(parts):
    root = build_graph(parts)
    return get_strongest_bridge(root)


def run_part2(parts):
    root = build_graph(parts)
    return get_longest_bridge(root)[0]


def parse_input(lines):
    return [tuple([int(c) for c in line.strip().split('/')] + [i]) for i, line in enumerate(lines) if line.strip()]


def main():
    with load_file.open_file('day24.input.txt') as f:
        parts = parse_input(f)
        #print(run_part1(parts))
        print(run_part2(parts))


if __name__ == '__main__':
    main()
