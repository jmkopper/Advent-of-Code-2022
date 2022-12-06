use ::std::collections::HashSet;

fn is_marker(seq: &str, req_len: usize) -> bool {
    let uniques: HashSet<char> = seq.chars().collect();
    return uniques.len() == req_len;
}

fn find_marker_seq(data_stream: &str, req_len: usize) -> usize {
    for i in 0..(data_stream.chars().count() - req_len - 1) {
        let next_n = &data_stream[i..i + req_len];
        if is_marker(next_n, req_len) {
            return i + req_len
        }
    }

    return 0;
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    println!("{:?}", find_marker_seq(&file_data, 14));
}
