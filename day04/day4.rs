fn get_lines(filename: &str) -> Option<Vec<String>> {
    let file_contents = std::fs::read_to_string(filename).unwrap();
    let lines: Vec<String> = file_contents
        .trim()
        .split("\n")
        .map(|x| x.to_string())
        .collect();
    Some(lines)
}

fn interval_str_to_int(interval: &str) -> (u32, u32) {
    let ints: Vec<&str> = interval.split("-").collect();
    (
        ints[0].parse::<u32>().unwrap(),
        ints[1].parse::<u32>().unwrap(),
    )
}

fn overlap(a: (u32, u32), b: (u32, u32)) -> bool {
    (a.0 <= b.0 && a.1 >= b.0) || (b.0 <= a.0 && b.1 >= a.0)
}

fn get_pair_interval(line: &str) -> ((u32, u32), (u32, u32)) {
    let ranges: Vec<&str> = line.split(",").collect();
    (
        interval_str_to_int(ranges[0]),
        interval_str_to_int(ranges[1]),
    )
}

fn main() {
    let file_lines = get_lines("input.txt").unwrap();
    let interval_strs: Vec<((u32, u32), (u32, u32))> =
        file_lines.iter().map(|x| get_pair_interval(x)).collect();
        
    let mut count_subints = 0;
    for interval in interval_strs.iter() {
        if overlap(interval.0, interval.1) {
            count_subints += 1;
        }
    }
    println!("{}", count_subints);
}
