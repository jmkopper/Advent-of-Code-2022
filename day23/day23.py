from collections import deque

DIRS8 = [(1,0), (0,1), (1, 1), (1, -1), (0, -1), (-1, 1), (-1, -1), (-1, 0)]
NORTH = [(-1, 0), (-1, 1), (-1, -1)]
SOUTH = [(1, 0), (1, 1), (1, -1)]
WEST = [(0, -1), (1, -1), (-1, -1)]
EAST = [(0, 1), (1, 1), (-1, 1)]

def neighbors(elves, elf, dir_set):
    return [d for d in dir_set if (elf[0] + d[0], elf[1] + d[1]) in elves]

def update_proposals(elves, elf, dir_order, width, height, proposals):
    for dir_set in dir_order:
        if not neighbors(elves, elf, dir_set):
            dest = (dir_set[0][0] + elf[0], dir_set[0][1] + elf[1])
            if dest in proposals:
                proposals[dest].append(elf)
            else:
                proposals[dest] = [elf]
            return

def run_step(elves, dir_order, width, height):
    proposals = dict()
    moves = 0
    for elf in elves:
        if not neighbors(elves, elf, DIRS8):
            continue
        update_proposals(elves, elf, dir_order, width, height, proposals)

    for dest, props in proposals.items():
        if len(props) == 1:
            elves.add(dest)
            elves.remove(props[0])
            moves += 1
    return False if moves == 0 else True

def run_many_steps(elves, width, height, num_steps):
    dir_order = deque([NORTH, SOUTH, WEST, EAST])
    for _ in range(num_steps):
        run_step(elves, dir_order, width, height)
        fst = dir_order.popleft()
        dir_order.append(fst)

def run_until_done(elves, width, height):
    dir_order = deque([NORTH, SOUTH, WEST, EAST])
    running = True
    count = 0
    while running:
        count += 1
        running = run_step(elves, dir_order, width, height)
        fst = dir_order.popleft()
        dir_order.append(fst)
    return count


def print_grid(elves, width, height):
    for row in range(height):
        for col in range(width):
            if (row, col) in elves:
                print('#', end='')
            else:
                print('.', end='')
        print()

def score_grid(elves):
    rows = [e[0] for e in elves]
    cols = [e[1] for e in elves]
    return (max(rows) - min(rows) + 1) * (max(cols) - min(cols) + 1) - len(elves)

def main():
    with open("input.txt") as f:
        grid = f.readlines()
    grid = [l.strip() for l in grid]
    elves = set()
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] == '#':
                elves.add((row, col))
    height = len(grid)
    width = len(grid[0])
    # run_many_steps(elves, width, height, 10)
    # print(f"part 1: {score_grid(elves)}")
    print(f"part 2: {run_until_done(elves, width, height)}")

if __name__ == "__main__":
    main()
