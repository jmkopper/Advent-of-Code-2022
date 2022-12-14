fn parse_line(xs: &str) -> Vec<(usize, usize)> {
    let coord_str: Vec<&str> = xs.trim().split(" -> ").collect();
    let mut coords: Vec<(usize, usize)> = Vec::new();
    for chunk in coord_str.iter() {
        let pair: Vec<&str> = chunk.split(",").collect();
        coords.push((pair[0].parse().unwrap(), pair[1].parse().unwrap()));
    }
    let mut rock_coords: Vec<(usize, usize)> = Vec::new();

    for i in 0..coords.len()-1 {
        if coords[i].0 == coords[i+1].0 {
            let (ymin, ymax) = (std::cmp::min(coords[i].1, coords[i+1].1), std::cmp::max(coords[i].1, coords[i+1].1));
            for y in ymin..=ymax {
                rock_coords.push((coords[i].0, y)); // will add duplicates at end points /shrug
            }
        } else {
            let (xmin, xmax) = (std::cmp::min(coords[i].0, coords[i+1].0), std::cmp::max(coords[i].0, coords[i+1].0));
            for x in xmin..=xmax {
                rock_coords.push((x, coords[i].1));
            }
        }
    }
    rock_coords
}

#[derive(Clone, Debug, PartialEq)]
enum Obj {
    Rock,
    Air,
    Sand,
}

#[derive(Debug)]
struct Cave {
    objects: Vec<Vec<Obj>>,
    sand_source: (usize, usize), 
}

fn build_cave(rock_coords: &Vec<(usize, usize)>) -> Cave {
    let xs: Vec<usize> = rock_coords.iter().map(|a| a.0).collect();
    let ys: Vec<usize> = rock_coords.iter().map(|a| a.1).collect();
    let minx = xs.iter().min().unwrap();
    let (miny, maxy) = (0, ys.iter().max().unwrap()); // force miny to zero because that's where the sand source is

    // Shift everything to save memory
    let width = 3*(maxy - miny + 5); // really just needs to be wider than it is tall
    let xshift = minx - width/3;
    let mut cave_objs = vec![vec![Obj::Air; width]; maxy - miny + 5];
    
    for (rock_x, rock_y) in rock_coords.iter() {
        cave_objs[*rock_y][rock_x - xshift] = Obj::Rock;
    }
    
    // fill in the floor
    for i in 0..width {
        cave_objs[maxy + 2][i] = Obj::Rock;
    }

    Cave{ objects: cave_objs, sand_source: (500-xshift, 0) }
}

fn simulate_sand_grain(cave: &mut Cave) -> bool {
    let mut sand_coord = cave.sand_source;

    let mut below = &cave.objects[sand_coord.1 + 1][sand_coord.0];
    let mut below_left = &cave.objects[sand_coord.1 + 1][sand_coord.0 - 1];
    let mut below_right = &cave.objects[sand_coord.1 + 1][sand_coord.0 + 1];

    while below == &Obj::Air || below_left == &Obj::Air || below_right == &Obj::Air {
        if below == &Obj::Air {
            sand_coord = (sand_coord.0, sand_coord.1 + 1);
        } else if below_left == &Obj::Air {
            sand_coord = (sand_coord.0 - 1, sand_coord.1 + 1);
        } else if below_right == &Obj::Air {
            sand_coord = (sand_coord.0 + 1, sand_coord.1 + 1);
        }

        below = &cave.objects[sand_coord.1 + 1][sand_coord.0];
        below_left = &cave.objects[sand_coord.1 + 1][sand_coord.0 - 1];
        below_right = &cave.objects[sand_coord.1 + 1][sand_coord.0 + 1];
    }

    
    if sand_coord == cave.sand_source {
        return true;
    }
    
    cave.objects[sand_coord.1][sand_coord.0] = Obj::Sand;
    return false
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.split("\n").collect();
    let rock_coords: Vec<(usize, usize)> = lines.into_iter().filter(|x| !x.is_empty()).map(|x| parse_line(x)).flatten().collect();
    let mut cave = build_cave(&rock_coords);
    let mut count = 0;
    let mut t = false;
    while !t {
        t = simulate_sand_grain(&mut cave);
        count += 1;
    }
    println!("part 2: {}", count);
}
