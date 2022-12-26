from collections import deque

def neighbors(x, y, z):
    return [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

def free_neighbors(pos, droplet, mins, maxs):
    free_neighbors = []
    for neighbor in neighbors(*pos):
        if pos not in droplet and all([neighbor[i] >= mins[i] and neighbor[i] <= maxs[i] for i in range(3)]):
            free_neighbors.append(neighbor)
    return free_neighbors

def surface_area(droplet):
    seen = set()
    area = 0
    for coords in droplet:
        area += 6
        seen.add(coords)
        for neighbor in neighbors(*coords):
            if neighbor in seen:
                area -= 2
    return area

def find_interior(coords):
    droplet = set(coords)
    visited = set()
    mins = (-1, -1, -1)
    maxs = tuple([max(c[i] for c in coords) for i in range(3)])
    queue = deque([mins])
    while len(queue) > 0:
        v = queue.popleft()
        for neighbor in free_neighbors(v, droplet, mins, maxs):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)
    interior = [(x, y, z) for x in range(maxs[0])
                   for y in range(maxs[1])
                   for z in range(maxs[2])
                   if (x, y, z) not in visited and (x, y, z) not in droplet]

    return interior


def main():
    with open("input.txt") as f:
        raw_data = f.readlines()
    lines = [tuple([int(x) for x in line.strip().split(",")]) for line in raw_data]
    total_area = surface_area(lines)
    print(f"Part 1: {total_area}")
    interior_area = surface_area(find_interior(lines))
    print(f"Part 2: {total_area - interior_area}")

if __name__ == "__main__":
    main()
