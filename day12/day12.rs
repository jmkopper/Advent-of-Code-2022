use std::collections::{HashMap, VecDeque};
use std::fs::read_to_string;

fn elevation(c: char) -> u32 {
    let val = match c {
        'E' => 'z',
        'S' => 'a',
        x => x,
    } as u32;
    val
}

fn get_neighbor_coords(row: usize, col: usize, nrows: usize, ncols: usize) -> Vec<(usize, usize)> {
    let mut neighbors: Vec<(usize, usize)> = Vec::new();
    if row < nrows - 1 {
        neighbors.push((row + 1, col));
    }
    if row > 0 {
        neighbors.push((row - 1, col));
    }
    if col < ncols - 1 {
        neighbors.push((row, col + 1))
    }
    if col > 0 {
        neighbors.push((row, col - 1))
    }
    neighbors
}

fn valid_neighbors(pos: (usize, usize), graph: &Vec<Vec<char>>) -> Vec<(usize, usize)> {
    let pos_char = graph[pos.0][pos.1];
    let neighbor_coords = get_neighbor_coords(pos.0, pos.1, graph.len(), graph[0].len());
    return neighbor_coords
        .into_iter()
        .filter(|x| elevation(pos_char) >= elevation(graph[x.0][x.1]) - 1)
        .collect();
}

fn bfs(graph: &Vec<Vec<char>>, start: (usize, usize), end: (usize, usize)) -> Option<usize> {
    let mut queue: VecDeque<(usize, usize)> = VecDeque::new();
    let mut visited: HashMap<(usize, usize), bool> = HashMap::new();
    let mut parent: HashMap<(usize, usize), (usize, usize)> = HashMap::new();

    visited.insert(start, true);
    queue.push_front(start);

    while !queue.is_empty() {
        let u = queue.pop_front().unwrap();
        if u == end {
            break;
        }
        for neighbor in valid_neighbors(u, graph) {
            if !visited.contains_key(&neighbor) {
                visited.insert(neighbor, true);
                parent.insert(neighbor, u);
                queue.push_back(neighbor);
            }
        }
    }

    if !parent.contains_key(&end) {
        return None;
    }

    let mut u = end;
    let mut dist = 0;
    while u != start {
        dist += 1;
        u = *parent.get(&u).unwrap()
    }

    return Some(dist);
}

fn main() {
    let raw_data = read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.trim().split("\n").collect();
    let graph: Vec<Vec<char>> = lines.iter().map(|x| x.chars().collect()).collect();
    let mut end: (usize, usize) = (0, 0);
    let mut a_coords: Vec<(usize, usize)> = Vec::new();
    for i in 0..graph.len() {
        for j in 0..graph[0].len() {
            if graph[i][j] == 'S' || graph[i][j] == 'a' {
                a_coords.push((i, j));
            }
            if graph[i][j] == 'E' {
                end = (i, j);
            }
        }
    }

    let a_dists: Vec<usize> = a_coords
        .into_iter()
        .map(|x| bfs(&graph, x, end))
        .flatten()
        .collect();
    println!("{:?}", a_dists.iter().min());
}
