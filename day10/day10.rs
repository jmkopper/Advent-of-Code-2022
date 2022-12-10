fn run_cmd(line: &str, cycles: &mut Vec<Vec<i32>>) {
    let chunks: Vec<&str> = line.trim().split(" ").collect();
    let last = cycles.last().unwrap().clone();

    cycles.push(vec![last[1], last[1]]);
    if chunks[0] == "addx" {
        let val = chunks[1].parse::<i32>().unwrap();
        cycles.push(vec![last[1], last[1] + val]);
    }
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = file_data.trim().split("\n").collect();
    let mut cycles = vec![vec![1, 1]];
    for line in lines.iter() {
        run_cmd(line, &mut cycles);
    }
    let mut sprite_pos: Vec<i32> = cycles.iter().map(|x| x[0]).collect();
    sprite_pos = sprite_pos[1..].to_vec();

    for i in 0..sprite_pos.len() {
        let draw_pos = i as i32 % 40;
        if draw_pos >= sprite_pos[i] - 1 && draw_pos <= sprite_pos[i] + 1 {
            print!("#");
        } else {
            print!(".");
        }
        if draw_pos == 39 {
            println!()
        }
    }
}
