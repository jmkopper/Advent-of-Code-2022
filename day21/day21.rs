use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Sub,
    Div,
    Mult,
    Eq,
    Just(i64),
}

#[derive(Debug, Clone, Copy)]
enum Output {
    Num(i64),
    Truth(bool),
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
    // let op = if name == "root" {
    //     Op::Eq
    // } else {
    //     op
    // };
    let children: Option<(String, String)> = if words.len() == 2 {
        None
    } else {
        Some((words[1].to_string(), words[3].to_string()))
    };

    return Node { name, children, op }
}

fn do_op(op: Op, fst: i64, snd: i64) -> Output {
    match op {
        Op::Add => Output::Num(fst + snd),
        Op::Sub => Output::Num(fst - snd),
        Op::Mult => Output::Num(fst * snd),
        Op::Div => Output::Num(fst / snd),
        Op::Just(x) => Output::Num(x),
        Op::Eq => Output::Truth(fst == snd)
    }
}

fn eval(node: &Node, graph: &HashMap<String, Node>, values: &mut HashMap<String, Output>) -> Output {
    if let Some(&x) = values.get(&node.name) {
        return x;
    }
    if let Op::Just(x) = node.op {
        return Output::Num(x);
    }
    let left = &node.children.as_ref().unwrap().0;
    let right = &node.children.as_ref().unwrap().1;
    if let (Some(&Output::Num(fst)), Some(&Output::Num(snd))) = (values.get(left), values.get(right)) {
        let v = do_op(node.op, fst, snd);
        values.insert(node.name.to_string(), v);
        return v;
    }

    // recurse
    let left_child = graph.get(left).unwrap();
    let right_child = graph.get(right).unwrap();
    if let (Output::Num(fst), Output::Num(snd)) = (eval(left_child, graph, values), eval(right_child, graph, values)) {
        let v = do_op(node.op, fst, snd);
        values.insert(node.name.to_string(), v);
    }
    return Output::Truth(false);
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.split("\n").filter(|x| !x.is_empty()).collect();
    let nodes: Vec<Node> = lines.iter().map(|x| parse_line(x)).collect();
    let graph: HashMap<String, Node> = HashMap::from_iter(nodes.into_iter().map(|x| (x.name.to_string(), x)));
    let mut values: HashMap<String, Output> = HashMap::new();
    let root = graph.get(&"root".to_string()).unwrap();
    println!("{:?}", eval(root, &graph, &mut values));
}
