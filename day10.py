
def run(lengths, list_size=256):
    data = [x for x in range(list_size)]
    skip_size = 0
    current_position = 0
    for length in lengths:
        for i in range(length // 2):
            swap(data, current_position + i, current_position + length - i - 1)
        current_position = (current_position + length + skip_size) % list_size
        skip_size += 1
    return data[0] * data[1]


def knot_hash(input_bytes, list_size=256):
    current_position = 0
    skip_size = 0
    data = [x for x in range(list_size)]
    lengths = get_lengths(input_bytes)
    for _ in range(64):
        current_position, skip_size, data = single_round(
            lengths, current_position, skip_size, data)
    return compress_hash(data)


def compress_hash(sparse_hash):
    dense_hash = []
    for i in range(0, len(sparse_hash), 16):
        element = 0
        for j in range(16):
            element ^= sparse_hash[i + j]
        dense_hash.append(element)
    return dense_hash


def single_round(lengths, current_position, skip_size, data):
    for length in lengths:
        for i in range(length // 2):
            swap(data, current_position + i, current_position + length - i - 1)
        current_position = (current_position + length + skip_size) % len(data)
        skip_size += 1
    return current_position, skip_size, data


def get_lengths(input_bytes):
    return [ord(x) for x in input_bytes] + [17, 31, 73, 47, 23]


def swap(l, a, b):
    a = a % len(l)
    b = b % len(l)
    x = l[a]
    l[a] = l[b]
    l[b] = x


def format_output(dense_hash):
    def fmt(x): return format(x, '02x')
    string_representation = map(fmt, dense_hash)
    return ''.join(string_representation)


def main():
    a = [230, 1, 2, 221, 97, 252, 168, 169, 57, 99, 0, 254, 181, 255, 235, 167]
    print(run(a))
    dense_hash = knot_hash(','.join(str(x) for x in a))
    print(format_output(dense_hash))


if __name__ == '__main__':
    main()
