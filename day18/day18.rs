use std::collections::{HashSet, VecDeque};

fn neighbors(cube: &Vec<i64>) -> Vec<Vec<i64>> {
    return vec![
        vec![cube[0] + 1, cube[1], cube[2]],
        vec![cube[0], cube[1] + 1, cube[2]],
        vec![cube[0], cube[1], cube[2] + 1],
        vec![cube[0] - 1, cube[1], cube[2]],
        vec![cube[0], cube[1] - 1, cube[2]],
        vec![cube[0], cube[1], cube[2] - 1],
    ];
}

fn surface_area(coords: &Vec<Vec<i64>>) -> i64 {
    let mut seen: HashSet<Vec<i64>> = HashSet::new();
    let mut area = 0;
    for cube in coords.iter() {
        area += 6;
        seen.insert(cube.to_vec());
        for neighbor in neighbors(cube) {
            if seen.contains(&neighbor) {
                area -= 2;
            }
        }
    }
    area
}

fn free_neighbors(
    pos: &Vec<i64>,
    droplet: &HashSet<Vec<i64>>,
    min: &Vec<i64>,
    max: &Vec<i64>,
) -> Vec<Vec<i64>> {
    let mut free_neighbors: Vec<Vec<i64>> = Vec::new();
    for neighbor in neighbors(pos).iter() {
        if !droplet.contains(neighbor)
            && neighbor
                .iter()
                .enumerate()
                .all(|(i, &x)| x <= max[i] && x >= min[i])
        {
            free_neighbors.push(neighbor.to_vec());
        }
    }
    free_neighbors
}

fn find_interior(coords: &Vec<Vec<i64>>) -> Vec<Vec<i64>> {
    let droplet: HashSet<Vec<i64>> = HashSet::from_iter(coords.iter().cloned());
    let min = vec![-1, -1, -1];
    let max = vec![
        coords.iter().map(|x| x[0]).max().unwrap() + 1,
        coords.iter().map(|x| x[1]).max().unwrap() + 1,
        coords.iter().map(|x| x[2]).max().unwrap() + 1,
    ];

    let mut queue: VecDeque<Vec<i64>> = VecDeque::from(vec![min.to_vec()]);
    let mut visited: HashSet<Vec<i64>> = HashSet::new();
    visited.insert(min.to_vec());
    while queue.len() > 0 {
        let v = queue.pop_front().unwrap();
        for neighbor in free_neighbors(&v, &droplet, &min, &max) {
            if !visited.contains(&neighbor) {
                visited.insert(neighbor.to_vec());
                queue.push_back(neighbor);
            }
        }
    }

    let mut interior: Vec<Vec<i64>> = Vec::new();
    for i in 0..max[0] {
        for j in 0..max[1] {
            for k in 0..max[2] {
                if !visited.contains(&vec![i, j, k]) && !droplet.contains(&vec![i, j, k]) {
                    interior.push(vec![i, j, k]);
                }
            }
        }
    }

    interior
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let coords: Vec<Vec<i64>> = raw_data
        .trim()
        .split("\n")
        .map(|x| x.split(",").map(|y| y.parse().unwrap()).collect())
        .collect();
    let total_area = surface_area(&coords);
    println!("part 1 {:?}", total_area);
    let interior = find_interior(&coords);
    let interior_area = surface_area(&interior);
    println!("part 2 {:?}", total_area - interior_area);
}
