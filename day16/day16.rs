use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Hash)]
struct Node {
    id: usize,
    name: String,
    neighbors: Vec<usize>,
    flow_rate: i64,
}

#[derive(Debug)]
struct Graph {
    nodes: Vec<Node>,
    nonzero_nodes: Vec<usize>,
}

fn parselines(lines: Vec<&str>) -> Graph {
    let mut node_names_map: HashMap<String, usize> = HashMap::new();
    let mut node_names: Vec<String> = Vec::new();
    let mut flow_rates: Vec<i64> = Vec::new();
    let mut neighbors: Vec<Vec<usize>> = Vec::new();

    for (i, line) in lines.iter().enumerate() {
        let halves: Vec<&str> = line.trim().split("; ").collect();
        let trimmed: Vec<&str> = halves[0].split(" has flow rate=").collect();
        node_names_map.insert(trimmed[0][6..].to_string(), i);
        node_names.push(trimmed[0][6..].to_string());
        flow_rates.push(trimmed[1].parse().unwrap());
    }

    for line in lines.iter() {
        let halves: Vec<&str> = line.trim().split("; ").collect();
        let split_string = match halves[1].chars().nth(21).unwrap() {
            's' => "tunnels lead to valves ",
            _ => "tunnel leads to valve ",
        };
        let trimmed: Vec<&str> = halves[1].split(split_string).collect();
        let neighbors_str: Vec<&str> = trimmed[1].split(", ").filter(|x| !x.is_empty()).collect();
        let node_neighbors: Vec<usize> = neighbors_str
            .iter()
            .map(|x| node_names_map.get(&x.to_string()).unwrap().to_owned())
            .collect();
        neighbors.push(node_neighbors);
    }

    let nodes: Vec<Node> = (0..node_names.len())
        .map(|i| Node {
            id: node_names_map.get(&node_names[i]).unwrap().to_owned(),
            name: node_names[i].clone(),
            neighbors: neighbors[i].clone(),
            flow_rate: flow_rates[i],
        })
        .collect();
    let nonzero_nodes = (0..nodes.len()).filter(|&i| nodes[i].flow_rate != 0 || nodes[i].name == "AA").collect();
    Graph { nodes, nonzero_nodes }
}

fn dists(graph: &Graph) -> HashMap<(usize, usize), usize> {
    let mut dists: HashMap<(usize, usize), usize> = HashMap::new();
    for node1 in graph.nodes.iter() {
        for node2 in graph.nodes.iter() {
            if node1.id == node2.id {
                dists.insert((node1.id, node1.id), 0);
            } else if node1.neighbors.contains(&node2.id) {
                dists.insert((node1.id, node2.id), 1);
            } else {
                dists.insert((node1.id, node2.id), 10000); // usize::MAX -- overflows
            }
        }
    }

    for i in 0..graph.nodes.len() {
        for j in 0..graph.nodes.len() {
            for k in 0..graph.nodes.len() {
                let triangle_dist = dists.get(&(i, k)).unwrap() + dists.get(&(j, k)).unwrap();
                let new_dist = std::cmp::min(dists.get(&(i, j)).unwrap(), &triangle_dist);
                dists.insert((i, j), *new_dist);
            }
        }
    }
    dists
}

fn dfs(
    graph: &Graph,
    dists: &HashMap<(usize, usize), usize>,
    root: &Node,
    time_remaining: i64,
    open_valves: &mut Vec<usize>,
    current_score: i64,
    memo: &mut HashMap<Vec<usize>, i64>,
) {
    let mut hashable = open_valves.to_vec();
    hashable.sort();
    let state_score = std::cmp::max(memo.get(&hashable).unwrap_or(&0), &current_score);
    memo.insert(hashable, *state_score);
    for node in graph.nonzero_nodes.iter() {
        let less_time = time_remaining - (*dists.get(&(root.id, *node)).unwrap() as i64) - 1;
        if less_time < 0 || open_valves.contains(&node) || node == &root.id {
            continue;
        }
        open_valves.push(*node);
        dfs(
            graph,
            dists,
            &graph.nodes[*node],
            less_time,
            open_valves,
            current_score + less_time * &graph.nodes[*node].flow_rate,
            memo,
        );
        open_valves.pop();
    }
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.trim().split("\n").collect();
    let graph = parselines(lines);
    let dists = dists(&graph);
    let mut open_valves: Vec<usize> = Vec::new();
    let mut memo: HashMap<Vec<usize>, i64> = HashMap::new();
    let mut start_node = &graph.nodes[0];
    for node in graph.nodes.iter() {
        if node.name == "AA".to_string() {
            start_node = node;
        }
    }
    dfs(
        &graph,
        &dists,
        start_node,
        26,
        &mut open_valves,
        0,
        &mut memo,
    );

    let mut best = 0;

    for (a, ascore) in &memo {
        for (b, bscore) in &memo {
            if !a.iter().any(|x| b.contains(x)) {
                best = std::cmp::max(best, ascore+bscore);
            }
        }
    }

    println!("{:?}", best);
}
