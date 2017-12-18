import load_file


def number_of_groups(graph):
    to_visit = set(graph.keys())
    number = 0
    while to_visit:
        size_of_group(graph, to_visit.pop(), to_visit)
        number += 1
    return number


def size_of_group(graph, input_pid, to_visit=set()):
    q = []
    q.append(input_pid)
    visited = set()
    visited.add(input_pid)
    size = 0
    while q:
        pid = q.pop(0)
        to_visit.discard(pid)
        for child in graph[pid]:
            if child not in visited:
                q.append(child)
                visited.add(child)
        size += 1
    return size


def parse_input(input_data):
    graph = {}

    def strip(x): return int(x.strip())
    for line in input_data:
        l = line.split(' <-> ')
        graph[int(l[0])] = list(map(strip, l[1].split(',')))
    return graph


def main():
    with load_file.open_file('day12.input.txt') as f:
        graph = parse_input(f)
        print(size_of_group(graph, 0))
        print(number_of_groups(graph))


if __name__ == '__main__':
    main()
