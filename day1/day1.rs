fn main() -> std::io::Result<()> {
    let file_contents = std::fs::read_to_string("./input.txt")?;
    let file_content_lines: Vec<&str> = file_contents.split("\n").collect();
    let mut temp_sum = 0;
    let mut top_3: [i32; 3] = [0, 0, 0];
    for line in file_content_lines.iter() {
        if line.is_empty() {
            top_3[0] = std::cmp::max(top_3[0], temp_sum);
            top_3.sort();
            temp_sum = 0;
        } else {
            temp_sum += line.parse::<i32>().unwrap();
        }
    }
    print!("{:?}", top_3.iter().sum::<i32>());
    Ok(())
}
