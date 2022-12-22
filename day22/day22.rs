#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum Dir {
    Left,
    Up,
    Right,
    Down,
}

fn turn(current: Dir, dir: Dir) -> Dir {
    if dir == Dir::Right {
        match current {
            Dir::Left => Dir::Up,
            Dir::Up => Dir::Right,
            Dir::Right => Dir::Down,
            Dir::Down => Dir::Left,
        }
    } else {
        match current {
            Dir::Left => Dir::Down,
            Dir::Down => Dir::Right,
            Dir::Right => Dir::Up,
            Dir::Up => Dir::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Obj {
    Wall,
    Air,
    Wrap,
}

#[derive(Debug)]
struct Instruction {
    dir: Dir,
    amount: usize,
}

impl std::fmt::Display for Obj {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let c = match self {
            Obj::Wall => "#",
            Obj::Air => ".",
            Obj::Wrap => " ",
        };
        write!(f, "{}", c)
    }
}

fn parse_instructions(line: &str) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    let mut current: String = "".to_string();
    for c in line.chars().into_iter() {
        if !c.is_alphabetic() {
            current.push(c);
        } else {
            let amount = current.parse::<usize>().unwrap();
            let dir: Dir = match c {
                'L' => Dir::Left,
                'R' => Dir::Right,
                _ => panic!("bad dir"),
            };
            let instruction = Instruction { dir, amount };
            instructions.push(instruction);
            current = "".to_string();
        }
    }
    return instructions;
}

fn parse_map(lines: &Vec<&str>) -> Vec<Vec<Obj>> {
    let width = lines.iter().map(|x| x.len()).max().unwrap();
    let height = lines.len();
    let mut cave: Vec<Vec<Obj>> = vec![vec![Obj::Wall; width]; height];
    for row in 0..lines.len() {
        for col in 0..width {
            if let Some(c) = lines[row].chars().nth(col) {
                let new: Obj = match c {
                    '#' => Obj::Wall,
                    '.' => Obj::Air,
                    _ => Obj::Wrap,
                };
                cave[row][col] = new;
            } else {
                cave[row][col] = Obj::Wrap;
            }
        }
    }
    return cave;
}

fn get_step(
    faces: &Vec<Vec<Vec<Obj>>>,
    pos: (usize, usize),
    facing: Dir,
    current_face: usize,
) -> ((usize, usize), Dir, usize) {
    let posi = (pos.0 as i64, pos.1 as i64);
    let heading: (i64, i64) = match facing {
        Dir::Right => (0, 1),
        Dir::Left => (0, -1),
        Dir::Up => (-1, 0),
        Dir::Down => (1, 0),
    };
    let new_pos = (posi.0 + heading.0, posi.1 + heading.1);
    let face = &faces[current_face];
    if new_pos.0 >= 0 && new_pos.0 < 50 as i64 && new_pos.1 >= 0 && new_pos.1 < 50 as i64 {
        // in bounds
        let new_posu = (new_pos.0 as usize, new_pos.1 as usize);
        if face[new_posu.0][new_posu.1] == Obj::Air {
            return (new_posu, facing, current_face);
        }
        if face[new_posu.0][new_posu.1] == Obj::Wall {
            return (pos, facing, current_face);
        }
    }

    let (new_face_num, new_facing) = next_face(current_face, facing);
    let (mut new_row, mut new_col) = pos;
    if facing == Dir::Right || facing == Dir::Left {
        if new_facing == Dir::Right {
            new_col = 0;
            if facing == Dir::Left {
                new_row = 49 - new_row;
            }
        }
        if new_facing == Dir::Up {
            new_col = new_row;
            new_row = 49;
        }
        if new_facing == Dir::Down {
            new_col = new_row;
            new_row = 0;
        }
        if new_facing == Dir::Left {
            new_col = 49;
            if facing == Dir::Right {
                new_row = 49 - new_row;
            }
        }
    }
    if facing == Dir::Up || facing == Dir::Down {
        if new_facing == Dir::Right {
            new_row = new_col;
            new_col = 0;
        }
        if new_facing == Dir::Up {
            new_row = 49;
            if facing == Dir::Down {
                new_col = 49-new_col;
            }
        }
        if new_facing == Dir::Down {
            new_row = 0;
            if facing == Dir::Up {
                new_col = 49-new_col;
            }
        }
        if new_facing == Dir::Left {
            new_row = new_col;
            new_col = 49;
        }
    }

    if faces[new_face_num][new_row][new_col] == Obj::Air {
        return ((new_row, new_col), new_facing, new_face_num);
    }

    return (pos, facing, current_face);
}

fn navigate_cave(
    cube: &Vec<Vec<Vec<Obj>>>,
    instructions: &Vec<Instruction>,
) -> ((usize, usize), Dir, usize) {
    let mut facing = Dir::Right;
    let mut pos = (0usize, 0usize); // row, col
    let mut face = 0;

    for instruction in instructions {
        for _ in 0..instruction.amount {
            (pos, facing, face) = get_step(cube, pos, facing, face);
        }
        facing = turn(facing, instruction.dir);
    }
    return (pos, facing, face);
}

fn password(dir: Dir, pos: (usize, usize)) -> usize {
    let facing_val = match dir {
        Dir::Right => 0,
        Dir::Down => 1,
        Dir::Left => 2,
        Dir::Up => 3,
    };
    return (pos.0 + 1) * 1000 + 4 * (pos.1 + 1) + facing_val;
}

fn next_face(start_face: usize, start_dir: Dir) -> (usize, Dir) {
    if start_face == 0 {
        match start_dir {
            Dir::Right => (1, Dir::Right),
            Dir::Up => (5, Dir::Right),
            Dir::Left => (3, Dir::Right),
            _ => (2, Dir::Down),
        }
    } else if start_face == 1 {
        match start_dir {
            Dir::Right => (4, Dir::Left),
            Dir::Up => (5, Dir::Up),
            Dir::Left => (0, Dir::Left),
            _ => (2, Dir::Left),
        }
    } else if start_face == 2 {
        match start_dir {
            Dir::Right => (1, Dir::Up),
            Dir::Up => (0, Dir::Up),
            Dir::Left => (3, Dir::Down),
            _ => (4, Dir::Down),
        }
    } else if start_face == 3 {
        match start_dir {
            Dir::Right => (4, Dir::Right),
            Dir::Up => (2, Dir::Right),
            Dir::Left => (0, Dir::Right),
            _ => (5, Dir::Down),
        }
    } else if start_face == 4 {
        match start_dir {
            Dir::Right => (1, Dir::Left),
            Dir::Up => (2, Dir::Up),
            Dir::Left => (3, Dir::Left),
            _ => (5, Dir::Left),
        }
    } else {
        match start_dir {
            Dir::Right => (4, Dir::Up),
            Dir::Up => (3, Dir::Up),
            Dir::Left => (0, Dir::Down),
            _ => (1, Dir::Down),
        }
    }
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let chunks: Vec<&str> = raw_data.split("\n\n").filter(|x| !x.is_empty()).collect();
    let mut instructions = parse_instructions(chunks[1]);
    let fake_instruction1 = Instruction { dir: Dir::Left, amount: 18 };
    let fake_instruction2 = Instruction { dir: Dir::Right, amount: 0 };
    instructions.push(fake_instruction1);
    instructions.push(fake_instruction2); // bad parser
    let map_lines: Vec<&str> = chunks[0].split("\n").filter(|x| !x.is_empty()).collect();
    let map = parse_map(&map_lines);

    let face1 = map[0..50].iter().map(|s| s[50..100].to_vec()).collect();
    let face2 = map[0..50].iter().map(|s| s[100..150].to_vec()).collect();
    let face3 = map[50..100].iter().map(|s| s[50..100].to_vec()).collect();
    let face4 = map[100..150].iter().map(|s| s[0..50].to_vec()).collect();
    let face5 = map[100..150].iter().map(|s| s[50..100].to_vec()).collect();
    let face6 = map[150..200].iter().map(|s| s[0..50].to_vec()).collect();
    let faces = vec![face1, face2, face3, face4, face5, face6];


    let (pos, facing, face) = navigate_cave(&faces, &instructions);
    println!("{:?} {:?} {}", pos, facing, face);
    println!("{:?}", password(facing, pos));
    // compute manually from here, you loser
}
