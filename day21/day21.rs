use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Div,
    Mult,
    Just(i64),
}

#[derive(Debug)]
struct Node {
    name: String,
    children: Option<(String, String)>,
    op: Op,
}

fn parse_line(line: &str) -> Node {
    let words: Vec<&str> = line.split(" ").collect();
    let name = words[0][0..4].to_string();
    let op = if words.len() == 2 {
        Op::Just(words[1].parse().unwrap())
    } else {
        match words[2] {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mult,
            "/" => Op::Div,
            _ => Op::Add,
        }
    };
    let children: Option<(String, String)> = if words.len() == 2 {
        None
    } else {
        Some((words[1].to_string(), words[3].to_string()))
    };

    return Node { name, children, op }
}

fn do_op(op: Op, fst: i64, snd: i64) -> i64 {
    match op {
        Op::Add => fst + snd,
        Op::Sub => fst - snd,
        Op::Mult => fst * snd,
        Op::Div => fst / snd,
        Op::Just(x) => x,
    }
}

fn eval(node: &Node, graph: &HashMap<String, Node>, values: &mut HashMap<String, i64>) -> i64 {
    if let Some(&x) = values.get(&node.name) {
        return x;
    }
    if let Op::Just(x) = node.op {
        return x;
    }
    let left = &node.children.as_ref().unwrap().0;
    let right = &node.children.as_ref().unwrap().1;
    if let (Some(&fst), Some(&snd)) = (values.get(left), values.get(right)) {
        let v = do_op(node.op, fst, snd);
        values.insert(node.name.to_string(), v);
        return v;
    }

    // recurse
    let left_child = graph.get(left).unwrap();
    let fst = eval(left_child, graph, values);
    let right_child = graph.get(right).unwrap();
    let snd = eval(right_child, graph, values);
    let v = do_op(node.op, fst, snd);
    values.insert(node.name.to_string(), v);
    return v;
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.split("\n").filter(|x| !x.is_empty()).collect();
    let nodes: Vec<Node> = lines.iter().map(|x| parse_line(x)).collect();
    let graph: HashMap<String, Node> = HashMap::from_iter(nodes.into_iter().map(|x| (x.name.to_string(), x)));
    let mut values: HashMap<String, i64> = HashMap::new();
    let root = graph.get(&"root".to_string()).unwrap();
    println!("{:?}", eval(root, &graph, &mut values));
}
