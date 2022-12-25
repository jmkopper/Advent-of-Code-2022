use std::collections::HashMap;

fn to_dec(s: &str, snafu: &HashMap<char, isize>) -> isize {
    s.chars()
        .rev()
        .enumerate()
        .map(|(p, c)| snafu.get(&c).unwrap() * 5_isize.pow(p as u32))
        .sum()
}

fn to_snafu(n: isize, snafu_inv: [(&str, isize); 6]) -> String {
    let mut snafu = "".to_string();
    let mut m = n;
    let mut extra = 0;
    while m > 0 {
        extra = snafu_inv[(m % 5 + extra) as usize].1;
        snafu.push_str(snafu_inv[(m % 5 + extra) as usize].0);
        m = m / 5;
    }
    if extra > 0 {
        snafu.push_str(snafu_inv[extra as usize].0);
    }
    return snafu.chars().rev().collect();
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let snafu: HashMap<char, isize> = HashMap::from([('1', 1), ('2', 2), ('0', 0), ('-', -1), ('=', -2)]);
    let snafu_inv: [(&str, isize); 6] = [("0", 0), ("1", 0), ("2", 0), ("=", 1), ("-", 1), ("0", 1)];
    println!( "{}", to_snafu(raw_data.split("\n").map(|x| to_dec(x, &snafu)).sum(),snafu_inv));
}
