use std::collections::HashSet;

fn parse_line(line: &str) -> (char, i32) {
    let split_line: Vec<&str> = line.split(" ").collect();
    let dir_char = split_line[0].chars().next().expect("empty string");
    let step_size: i32 = split_line[1].parse().unwrap();
    (dir_char, step_size)
}

struct State {
    knots: Vec<(i32, i32)>,
}

impl State {
    fn take_step(&mut self, direction: &char) {
        match direction {
            'D' => self.knots[0].1 -= 1,
            'L' => self.knots[0].0 -= 1,
            'R' => self.knots[0].0 += 1,
            'U' => self.knots[0].1 += 1,
            _ => panic!("Invalid direction"),
        }
        self.update_body();
    }

    fn dist(&mut self, first: usize, second: usize) -> (i32, i32) {
        (
            self.knots[first].0 - self.knots[second].0,
            self.knots[first].1 - self.knots[second].1,
        )
    }

    fn update_body(&mut self) {
        for i in 1..self.knots.len() {
            let (x_dist, y_dist) = self.dist(i - 1, i);
            if x_dist.abs() == 2 || y_dist.abs() == 2 {
                self.knots[i].0 += x_dist / std::cmp::max(x_dist.abs(), 1);
                self.knots[i].1 += y_dist / std::cmp::max(y_dist.abs(), 1);
            }
        }
    }

    fn perform_step(&mut self, instruction: &(char, i32), tail_visited: &mut HashSet<(i32, i32)>) {
        for _ in 0..instruction.1 {
            self.take_step(&instruction.0);
            tail_visited.insert(*self.knots.last().unwrap());
        }
    }
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = file_data.trim().split("\n").collect();
    let moves: Vec<(char, i32)> = lines.iter().map(|x| parse_line(x)).collect();

    let mut state = State {
        knots: vec![
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ],
    };
    let mut tail_visited: HashSet<(i32, i32)> = HashSet::from([(0, 0)]);

    for step in moves.iter() {
        state.perform_step(step, &mut tail_visited);
    }

    println!("{}", tail_visited.len());
}
