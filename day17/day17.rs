use std::collections::HashMap;

fn parse_char(c: char) -> Movement {
    match c {
        '<' => Movement::Left,
        '>' => Movement::Right,
        _ => Movement::Down,
    }
}

enum Rock {
    Minus,
    Plus,
    J,
    I,
    Square,
}

fn rock_shape(rock: &Rock) -> Vec<(usize, usize)> {
    match rock {
        Rock::Minus => vec![(2, 0), (3, 0), (4, 0), (5, 0)],
        Rock::Plus => vec![(3,0), (2,1), (3,1), (4,1), (3, 2)],
        Rock::J => vec![(2,0), (3,0), (4,0), (4,1), (4,2)],
        Rock::I => vec![(2,0), (2,1), (2,2), (2,3)],
        Rock::Square => vec![(2,0), (3,0), (2,1), (3,1)],
    }
}

#[derive(Clone, Copy, Debug)]
enum Movement {
    Left,
    Right,
    Down,
}

#[derive(Clone, PartialEq, Debug)]
enum Obj {
    StoppedRock,
    Air,
}

struct Cave {
    objects: Vec<Vec<Obj>>,
    moving_obj_coords: Vec<(usize, usize)>,
    width: usize,
    highest_rock: usize,
}

impl Cave {
    fn simulate_movement(&mut self, movement: Movement) -> bool {
        match movement {
            Movement::Left => {
                if self.moving_obj_coords.iter().any(|x| x.0 == 0 || self.objects[x.1][x.0-1] == Obj::StoppedRock) {
                } else {
                    self.moving_obj_coords = self.moving_obj_coords.iter().map(|x| (x.0-1, x.1)).collect();
                }
            },
            Movement::Right => {
                if self.moving_obj_coords.iter().any(|x| x.0 == self.width-1 || self.objects[x.1][x.0+1] == Obj::StoppedRock) {
                } else {
                    self.moving_obj_coords = self.moving_obj_coords.iter().map(|x| (x.0+1, x.1)).collect();
                }
            },
            Movement::Down => {
                if self.moving_obj_coords.iter().any(|x| self.objects[x.1-1][x.0] == Obj::StoppedRock) {
                    for coords in self.moving_obj_coords.iter() {
                        self.objects[coords.1][coords.0] = Obj::StoppedRock;
                    }
                    self.moving_obj_coords = Vec::new();
                    for row in self.highest_rock..self.objects.len() {
                        for obj in self.objects[row].iter() {
                            if obj == &Obj::StoppedRock {
                                self.highest_rock = row;
                            }
                        }
                    }
                    return false
                } else {
                    self.moving_obj_coords = self.moving_obj_coords.iter().map(|x| (x.0, x.1-1)).collect();
                }
            }
        }

        return true
    }

    fn simulate(&mut self, instructions: &Vec<Movement>, num_runs: usize) -> HashMap<Vec<usize>, Vec<usize>>{
        let mut current = Rock::Minus;
        let mut dir_idx = 0;
        let mut rock_patterns: HashMap<Vec<usize>, Vec<usize>> = HashMap::new();
        for run in 1..num_runs+1 {
            let new_moving_coords = rock_shape(&current);
            for _ in self.objects.len()..10+self.highest_rock {
                self.objects.push(vec![Obj::Air; self.width]);
            }
            for row in new_moving_coords.iter() {
                self.moving_obj_coords.push((row.0, row.1 + self.highest_rock + 4));
            }
    
            let pattern = self.rock_pattern();
            let mut seen = vec![dir_idx % instructions.len(), run];
            match rock_patterns.get(&pattern) {
                Some(v) => {
                    seen.extend(v);
                },
                None => {},
            }
            rock_patterns.insert(pattern, seen);
            
            let mut in_motion = true;
            while in_motion {


                dir_idx = dir_idx % instructions.len();
                let dir = instructions[dir_idx];
                in_motion = self.simulate_movement(dir);
                dir_idx += 1;
                if !in_motion {
                    break;
                }
                in_motion = self.simulate_movement(Movement::Down);
            }
    
            current = match current {
                Rock::Minus => Rock::Plus,
                Rock::Plus => Rock::J,
                Rock::J => Rock::I,
                Rock::I => Rock::Square,
                Rock::Square => Rock::Minus,
            };
        }

        return rock_patterns
    }

    fn rock_pattern(&self) -> Vec<usize> {
        let mut rock_pattern: Vec<usize> = vec![std::usize::MAX; self.width];
        let mut row = self.highest_rock;
        while rock_pattern.iter().any(|&x| x == std::usize::MAX) {
            for (i, obj) in self.objects[row].iter().enumerate() {
                if obj == &Obj::StoppedRock && rock_pattern[i] == std::usize::MAX {
                    rock_pattern[i] = self.highest_rock-row;
                }
            }
            if row > 0 {
                row -= 1;
            }
        }
        return rock_pattern
    }
}

impl std::fmt::Display for Cave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut objs = self.objects.clone();
        objs.reverse();
        let mut pretty_str = "".to_string();
        for row in objs.iter() {
            for obj in row.iter() {
                match obj {
                    Obj::StoppedRock => pretty_str.push_str("#"),
                    Obj::Air => pretty_str.push_str("."),
                }
            }
            pretty_str.push_str("\n")
        }

        return write!(f, "{}", pretty_str);
    }
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let instructions: Vec<Movement> = raw_data.trim().chars().map(|c| parse_char(c)).collect();
    let initial_objs: Vec<Vec<Obj>> = vec![vec![Obj::StoppedRock; 7], vec![Obj::Air; 7], vec![Obj::Air; 7], vec![Obj::Air; 7]];
    let mut cave = Cave { objects: initial_objs, moving_obj_coords: Vec::new(), width: 7, highest_rock: 0};
    let patterns = cave.simulate(&instructions, 10000); //finished part 2 by hand
}
