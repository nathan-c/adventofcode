import load_file


class GraphNode(object):
    def __init__(self, part, next_port):
        self.parent = None
        self.children = []
        self.part = part
        self.weight = part[0] + part[1]
        self.next_port = next_port

    def add_child(self, child):
        if child.parent:
            raise ValueError('New child already has a parent!')
        self.children.append(child)
        child.parent = self
        child.weight += self.weight

    def can_connect(self, part):
        return True if part[0] == self.part[self.next_port] or part[1] == self.part[self.next_port] else False


def build_graph(parts):
    root = GraphNode((0, 0), 0)
    partial_parts = {'': parts}
    _build_graph(root, [], partial_parts)
    return root


def get_strongest_bridge(node):
    strongest = node.weight
    for child in node.children:
        new_strongest = get_strongest_bridge(child)
        if new_strongest > strongest:
            strongest = new_strongest
    return strongest


def _build_graph(graph, path, partial_parts):
    parts = partial_parts[','.join(path)]
    for i, part in enumerate(parts):
        if graph.can_connect(part):
            next_port = 1 if part[0] == graph.part[graph.next_port] else 0
            new_node = GraphNode(part, next_port)
            graph.add_child(new_node)
            child_path = sorted(path + [str(part[2])])
            child_path_string = ','.join(child_path)
            if child_path_string not in partial_parts:
                new_parts = parts[:i] + parts[i + 1:]
                partial_parts[child_path_string] = new_parts

            _build_graph(new_node, child_path, partial_parts)


def run_part1(parts):
    root = build_graph(parts)
    return get_strongest_bridge(root)


def parse_input(lines):
    return [[int(c) for c in line.strip().split('/')] + [i] for i, line in enumerate(lines) if line.strip()]


def main():
    with load_file.open_file('day24.input.txt') as f:
        parts = parse_input(f)
        print(run_part1(parts))


if __name__ == '__main__':
    main()
