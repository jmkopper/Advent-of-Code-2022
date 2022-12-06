use std::collections::HashSet;

fn is_marker(seq: &str, req_len: usize) -> bool {
    seq.chars().collect::<HashSet<_>>().len() == req_len
}

fn find_marker_seq(data_stream: &str, req_len: usize) -> usize {
    (0..(data_stream.chars().count() - req_len - 1))
        .find(|&i| is_marker(&data_stream[i..i + req_len], req_len))
        .map_or(0, |i| i + req_len)
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    println!("{:?}", find_marker_seq(&file_data, 14));
}
