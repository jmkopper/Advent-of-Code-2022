fn parse_crane_diagram(diagram: &str) -> Vec<Vec<char>> {
    let mut lines: Vec<&str> = diagram.split("\n").collect();
    lines.pop();
    lines.reverse();
    let mut rows: Vec<Vec<char>> = Vec::new();
    for line in lines.iter() {
        let mut row: Vec<char> = Vec::new();
        let mut i = 0;
        while i < line.len() {
            if line.chars().nth(i) == Some('[') {
                row.push(line.chars().nth(i + 1).unwrap());
            } else {
                row.push(' ');
            }

            i += 4;
        }
        rows.push(row);
    }

    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _i in 0..rows[0].len() {
        let stack: Vec<char> = Vec::new();
        stacks.push(stack);
    }

    for i in 0..rows[0].len() {
        for row in rows.iter() {
            if row[i] != ' ' { stacks[i].push(row[i]); }
        }
    }

    return stacks;
}

struct Instruction {
    quantity: usize,
    origin: usize,
    destination: usize,
}

fn parse_instruction(instruction: &str) -> Instruction {
    let words: Vec<&str> = instruction.split(" ").collect();
    Instruction {
        quantity: (words[1].parse::<usize>().unwrap()),
        origin: (words[3].parse::<usize>().unwrap()-1),
        destination: (words[5].parse::<usize>().unwrap()-1),
    }
}

fn parse_instructions(instructions: &str) -> Vec<Instruction> {
    let lines: Vec<&str> = instructions.split("\n").collect();
    return lines.iter().map(|x| parse_instruction(x)).collect();
}

fn perform_instruction(instruction: &Instruction, stacks: &mut Vec<Vec<char>>) {
    let mut moved_crates: Vec<char> = Vec::new();
    for i in 0..instruction.quantity {
        let moved_crate = stacks[instruction.origin].pop().unwrap();
        moved_crates.push(moved_crate);
    }
    moved_crates.reverse();
    for c in moved_crates.iter() {
        stacks[instruction.destination].push(*c);
    }
}

fn stack_tops(stacks: Vec<Vec<char>>) -> String {
    let mut tops = String::new();
    for stack in stacks.iter() {
        tops.push(stack.last().copied().unwrap());
    }
    tops
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let file_parts: Vec<&str> = file_data.split("\n\n").collect();
    let mut stacks = parse_crane_diagram(file_parts[0]);
    let instructions = parse_instructions(file_parts[1].trim());
    for instruction in instructions.iter() {
        perform_instruction(&instruction, &mut stacks);
    }
    println!("{:?}", stack_tops(stacks));
}
