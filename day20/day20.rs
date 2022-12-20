fn mix_perm(data: &Vec<i64>, num_mixes: usize) -> Vec<usize> {
    let mut queue = Vec::from_iter(0..data.len());
    for _ in 0..num_mixes {
        for (i, &val) in data.iter().enumerate() {
            let pos = queue.iter().position(|x| *x == i).unwrap();
            let new_idx_i64 = (pos as i64 + val) % (data.len() as i64 - 1);
            // annoying indexing
            let new_idx = if new_idx_i64 < 0 {
                (data.len() as i64 + new_idx_i64 - 1) as usize
            } else {
                new_idx_i64 as usize
            };

            let tmp = queue.remove(pos);
            queue.insert(new_idx, tmp)
        }
    }
    queue
}

fn find_coords(decoded: &Vec<i64>) -> Vec<i64> {
    let zero_pos = decoded.iter().position(|&x| x == 0).unwrap();
    return vec![
        decoded[(zero_pos + 1000) % decoded.len()],
        decoded[(zero_pos + 2000) % decoded.len()],
        decoded[(zero_pos + 3000) % decoded.len()],
    ];
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<i64> = raw_data
        .split("\n")
        .filter(|x| !x.is_empty())
        .map(|x| x.parse().unwrap())
        .collect();

    let positions_1 = mix_perm(&lines, 1);
    let decoded_1 = (0..lines.len()).map(|x| lines[positions_1[x]]).collect();
    println!("Part 1: {:?}", find_coords(&decoded_1).iter().sum::<i64>());

    const DECRYPTION_KEY: i64 = 811589153;
    let scaled = lines.iter().map(|x| x * DECRYPTION_KEY).collect();
    let positions_2 = mix_perm(&scaled, 10);
    let decoded_2 = (0..lines.len()).map(|x| scaled[positions_2[x]]).collect();
    println!("Part 2: {:?}", find_coords(&decoded_2).iter().sum::<i64>());
}
