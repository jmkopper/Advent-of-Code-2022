from queue import Queue
from collections import Set


@value
struct Point(KeyElement):
    var r: Int
    var c: Int

    fn __hash__(self) -> Int:
        return (hash(self.r) ^ hash(self.c)) % 10_000

    fn __eq__(self, other: Self) -> Bool:
        return self.r == other.r and self.c == other.c

    fn __ne__(self, other: Self) -> Bool:
        return not self.__eq__(other)


@always_inline
fn height(c: String) -> Int:
    if c == "S":
        return ord("a")
    if c == "E":
        return ord("z")
    return ord(c)


fn get_neighbor_coords(p: Point, nrows: Int, ncols: Int) -> List[Point]:
    var neighbors = List[Point]()
    if p.r > 0:
        neighbors.append(Point(p.r - 1, p.c))
    if p.c > 0:
        neighbors.append(Point(p.r, p.c - 1))
    if p.r < nrows - 1:
        neighbors.append(Point(p.r + 1, p.c))
    if p.c < ncols - 1:
        neighbors.append(Point(p.r, p.c + 1))
    return neighbors


fn valid_neighbors(p: Point, graph: List[String]) -> List[Point]:
    var valid_neighbors = List[Point]()
    var height_here = height(graph[p.r][p.c])
    var neighbors = get_neighbor_coords(p, nrows=len(graph), ncols=len(graph[0]))
    for i in range(len(neighbors)):
        var neighbor = neighbors[i]
        var there = graph[neighbor.r][neighbor.c]
        if height_here >= height(there) - 1:
            valid_neighbors.append(neighbor)
    return valid_neighbors


fn bfs(graph: List[String], start: Point, end: Point) -> Int:
    var enqueued = Set[Point]()
    var q = Queue[Point]()
    var dist = 1 << 31
    var level = 0

    q.push_back(start)
    enqueued.add(start)

    while len(q) > 0:
        var level_size = len(q)

        while level_size > 0:
            var u = q.pop_front()
            if u == end:
                return level

            var neighbors = valid_neighbors(u, graph)
            for i in range(len(neighbors)):
                var neighbor = neighbors[i]
                if neighbor not in enqueued:
                    q.push_back(neighbor)
                    enqueued.add(neighbor)
            level_size -= 1
        level += 1

    return dist


fn main() raises:
    var contents: String
    with open("src/input.txt", "r") as file:
        contents = file.read()
    var lines = contents.strip().split("\n")
    var start = Point(0, 0)
    var end = Point(0, 0)
    var all_as = List[Point]()
    for r in range(len(lines)):
        var line = lines[r]
        for c in range(len(line)):
            if lines[r][c] == "S":
                start.r = r
                start.c = c
            if lines[r][c] == "E":
                end.r = r
                end.c = c
            if lines[r][c] == "a":
                all_as.append(Point(r, c))

    print("Part 1: ", bfs(lines, start, end))
    var min_d = 1 << 31
    for i in range(len(all_as)):
        var new_start = all_as[i]
        var dist = bfs(lines, new_start, end)
        if min_d > dist:
            min_d = dist
    print("Part 2: ", min_d)
