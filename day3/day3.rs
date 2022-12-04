use std::collections::HashSet;

fn get_trios(lines: Vec<String>) -> Vec<(String, String, String)> {
    let mut trios: Vec<(String, String, String)> = Vec::new();
    for i in (0..lines.len()).step_by(3) {
        trios.push((lines[i].to_string(), lines[i+1].to_string(), lines[i+2].to_string()));
    }
    trios
}

fn get_badge(trio: &(String, String, String)) -> char {
    let (first, second, third) = trio;
    let mut first_items: HashSet<char> = HashSet::new();
    let mut second_items: HashSet<char> = HashSet::new();
    for c in first.chars() {
        first_items.insert(c);
    }
    for c in second.chars() {
        if first_items.contains(&c) {
            second_items.insert(c);
        }
    }
    for c in third.chars() {
        if second_items.contains(&c) {
            return c;
        }
    }
    '!'
}

fn get_lines(filename: &str) -> Option<Vec<String>> {
    let file_contents = std::fs::read_to_string(filename).unwrap();
    let lines: Vec<String> = file_contents.trim().split("\n").map(|x| x.to_string()).collect();
    Some(lines)
}


fn item_priority(c: char) -> u32 {
    let ascii_val = c as u32;
    if ascii_val <= 90 {
        return ascii_val - 38;
    } else {
        return ascii_val - 96;
    }
}

fn main() {
    let file_lines = get_lines("input.txt").unwrap();
    let trios = get_trios(file_lines);
    let mut badges: Vec<char> = Vec::new();
    for trio in trios.iter() {
        badges.push(get_badge(trio));
    }
    println!("{:?}", badges);
    let priorities: Vec<u32> = badges.iter().map(|x| item_priority(*x)).collect();
    println!("{:?}", priorities.iter().sum::<u32>());
}
