from collections import deque

STEP_DIRS = [(1,0), (0,1), (-1, 0), (0, -1), (0, 0)]

def next_pos(shape, pos, width, height):
    if shape == '>':
        return (pos[0], pos[1]+1) if pos[1] < width - 2 else (pos[0], 1)
    if shape == '<':
        return (pos[0], pos[1]-1) if pos[1] > 1 else (pos[0], width-2)
    if shape == 'v':
        return (pos[0]+1, pos[1]) if pos[0] < height - 2 else (1, pos[1])
    if shape == '^':
        return (pos[0]-1, pos[1]) if pos[0] > 1 else (height-2, pos[1])

def step(map):
    next_map = [[[] for _ in range(len(map[0]))] for _ in range(len(map))]
    for row in range(len(map)):
        for col in range(len(map[0])):
            for shape in map[row][col]:
                if shape == '#':
                    next_map[row][col] = '#'
                    continue
                npos = next_pos(shape, (row, col), len(map[0]), len(map))
                if npos:
                    next_map[npos[0]][npos[1]].append(shape)
    return next_map

def safe_neighbors(map, pos, width, height, iter):
    return [(dy + pos[0], dx + pos[1], iter) for (dy, dx) in STEP_DIRS if
        0 <= (pos[0] + dy) < height and
        0 <= (pos[1] + dx) < width and
        not map[pos[0] + dy][pos[1] + dx]]

def build_graph(map, max_iter):
    nodes = {}
    for i in range(max_iter):
        next_map = step(map)
        for row in range(len(map)):
            for col in range(len(map[0])):
                if not map[row][col]:
                    sn =  safe_neighbors(next_map, (row, col), len(map[0]), len(map), i+1)
                    if sn:
                        nodes[(row, col, i)] = sn
        map = next_map
    return nodes
                    
def get_paths(graph, start, end, flippy_flops):
    queue = deque([start])
    parents = {}
    while queue:
        q = queue.popleft()
        if q not in graph:
            continue
        for neighbor in graph[q]:
            if (q[0], q[1]) == end:
                return q if flippy_flops == 0 else get_paths(graph, q, (start[0], start[1]), flippy_flops-1)
            if neighbor not in parents:
                queue.append(neighbor)
                parents[neighbor] = q

def main():
    with open("input.txt") as f:
            raw_data = f.readlines()
    lines = [line.strip() for line in raw_data]
    map = [[[x] for x in row] for row in lines]
    for row in range(len(map)):
        for col in range(len(map[0])):
            if map[row][col] == ['.']:
                map[row][col] = []
    start = lines[0].index('.')
    end = lines[-1].index('.')    
    nodes = build_graph(map, 1000)
    print("Part 1: ", get_paths(nodes, (0, start, 0), (len(map) - 1, end), 0)[2])
    print("Part 2: ", get_paths(nodes, (0, start, 0), (len(map) - 1, end), 2)[2])


if __name__ == '__main__':
    main()
