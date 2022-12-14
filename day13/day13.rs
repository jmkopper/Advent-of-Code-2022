#[derive(Debug, PartialEq, Eq, Ord)]
enum NestedList {
    Number(usize),
    List(Vec<NestedList>),
}

impl PartialOrd for NestedList {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (NestedList::Number(x), NestedList::Number(y)) => x.partial_cmp(y),
            (NestedList::List(_), NestedList::Number(y)) => {
                self.partial_cmp(&NestedList::List(vec![NestedList::Number(*y)]))
            }
            (NestedList::Number(x), NestedList::List(y)) => {
                vec![NestedList::Number(*x)].partial_cmp(y)
            }
            (NestedList::List(x), NestedList::List(y)) => x.partial_cmp(y),
        }
    }
}

fn matching_bracket_index(xs: &[String]) -> usize {
    let mut depth = 1;
    let mut i = 0;
    while depth > 0 {
        if xs[i].as_str() == "]" {
            depth -= 1
        }
        if xs[i].as_str() == "[" {
            depth += 1;
        }
        i += 1;
    }
    i
}

fn split_brackets_nums(xs: &str) -> Vec<String> {
    let mut res: Vec<String> = Vec::new();
    let mut current = "".to_string();
    for c in xs.chars() {
        if c == ',' {
            if current.len() > 0 {
                res.push(current);
            }
            current = "".to_string();
        } else if c == '[' || c == ']' {
            if current.len() > 0 {
                res.push(current);
            }
            res.push(format!("{}", c));
            current = "".to_string();
        } else {
            current = format!("{}{}", current, c);
        }
    }
    res
}

fn split_str_to_nested(xs: &[String]) -> NestedList {
    let mut nesteds: Vec<NestedList> = Vec::new();

    if xs == ["[", "]"] {
        return NestedList::List(nesteds);
    }

    let mut i = 0;
    while i < xs.len() {
        match xs[i].as_str() {
            "[" => {
                let matching_idx = i + matching_bracket_index(&xs[i + 1..]);
                let substr = &xs[i + 1..matching_idx];
                nesteds.push(split_str_to_nested(&substr));
                i = matching_idx - 1;
            }
            "]" => {}
            _ => {
                nesteds.push(NestedList::Number(xs[i].parse().unwrap()));
            }
        }
        i += 1;
    }
    NestedList::List(nesteds)
}

fn nested_from_str(xs: &str) -> NestedList {
    split_str_to_nested(&split_brackets_nums(xs))
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let pair_blocks: Vec<&str> = raw_data.trim().split("\n\n").collect();
    let packet_str: Vec<&str> = pair_blocks
        .iter()
        .map(|x| x.split("\n").collect::<Vec<&str>>())
        .flatten()
        .collect();
    let mut packets: Vec<NestedList> = packet_str.iter().map(|x| nested_from_str(x)).collect();

    let mut sum = 0;
    let mut i = 0;
    while i < packets.len() {
        if packets[i] < packets[i + 1] {
            sum += 1 + i / 2;
        }
        i += 2;
    }

    println!("part 1: {}", sum);

    packets.push(nested_from_str("[[2]]"));
    packets.push(nested_from_str("[[6]]"));
    packets.sort();

    let mut pos2 = 0;
    let mut pos6 = 0;

    for i in 0..packets.len() {
        if packets[i] == nested_from_str("[[2]]") {
            pos2 = 1 + i;
        }

        if packets[i] == nested_from_str("[[6]]") {
            pos6 = 1 + i;
        }
    }

    println!("part 2: {}", pos2 * pos6);
}
