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
        values.insert(node.name.to_string(), x);
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

fn depends_on(node: &Node, target: &Node, graph: &HashMap<String, Node>) -> bool {
    if node.name == target.name {
        return true;
    }

    if let Some((left, right)) = &node.children {
        let left_child = graph.get(left).unwrap();
        let right_child = graph.get(right).unwrap();
        return depends_on(left_child, target, graph) || depends_on(right_child, target, graph)
    }

    return false
}

fn get_yell(node: &Node, graph: &HashMap<String, Node>, known_vals: &HashMap<String, i64>, unknown_vals: &mut HashMap<String, String>) -> String {
    if node.name == "humn".to_string() {
        unknown_vals.insert("humn".to_string(), "x".to_string());
        return "x".to_string();
    }

    if let Some(val) = known_vals.get(&node.name) {
        unknown_vals.insert(node.name.to_string(), val.to_string());
        return val.to_string();
    }

    let left = &node.children.as_ref().unwrap().0;
    let right = &node.children.as_ref().unwrap().1;
    let mut cur_string = "(".to_string();

    if let Some(lval) = unknown_vals.get(left) {
        cur_string.push_str(&lval);
    } else {
        let left_child = graph.get(left).unwrap();
        cur_string.push_str(&get_yell(left_child, graph, known_vals, unknown_vals));
    }

    let op_str = match node.op {
        Op::Add => "+",
        Op::Sub => "-",
        Op::Mult => "*",
        Op::Div => "/",
        Op::Just(_) => "[WEIRD JUST VAL]",
    };

    cur_string.push_str(op_str);

    if let Some(rval) = unknown_vals.get(right) {
        cur_string.push_str(&rval);
    } else {
        let right_child = graph.get(right).unwrap();
        cur_string.push_str(&get_yell(right_child, graph, known_vals, unknown_vals));
    }

    cur_string.push_str(")");

    return cur_string
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.split("\n").filter(|x| !x.is_empty()).collect();
    let nodes: Vec<Node> = lines.iter().map(|x| parse_line(x)).collect();
    let mut graph: HashMap<String, Node> = HashMap::from_iter(nodes.into_iter().map(|x| (x.name.to_string(), x)));
    let mut values: HashMap<String, i64> = HashMap::new();
    let root = graph.get(&"root".to_string()).unwrap();

    println!("Part 1: {:?}", eval(root, &graph, &mut values));

    let (root_left, root_right) = (root.children.as_ref().unwrap().0.to_string(), root.children.as_ref().unwrap().1.to_string());
    let fake_root = Node { name: "root".to_string(), children: Some((root_left, root_right)), op: Op::Sub };
    let nodes: Vec<Node> = lines.iter().map(|x| parse_line(x)).collect();
    let humn = Node { name: "humn".to_string(), children: None, op: Op::Just(0) };
    let depends_on_humn: Vec<Node> = nodes.into_iter().filter(|x| depends_on(x, &humn, &graph)).collect();
    for dependent in depends_on_humn.iter() {
        values.remove(&dependent.name);
    }
    graph.insert("root".to_string(), fake_root);
    graph.insert("humn".to_string(), humn);
    let mut unknown_vals: HashMap<String, String> = HashMap::new();

    println!("Part 2: {:?}", get_yell(graph.get(&"root".to_string()).unwrap(), &graph, &values, &mut unknown_vals))
}
