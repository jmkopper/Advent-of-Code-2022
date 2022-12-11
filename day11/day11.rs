struct Monkey {
    items: Vec<usize>,
    operation: Box<dyn Fn(usize) -> usize>,
    test: usize,
    targets: (usize, usize),
    inspections: usize,
}

struct MonkeySet {
    monkeys: Vec<Monkey>,
}

impl MonkeySet {
    fn do_turn(&mut self, monkey_id: usize) {
        let monkey = &mut self.monkeys[monkey_id];
        let mut item_adds: Vec<(usize, usize)> = Vec::new(); // Borrow checker weirdness
        for old in monkey.items.iter() {
            monkey.inspections += 1;
            let new = (monkey.operation)(*old) % (11 * 7 * 3 * 5 * 17 * 13 * 19 * 2);
            let target = match new % monkey.test {
                0 => monkey.targets.0,
                _ => monkey.targets.1,
            };
            item_adds.push((target, new));
        }
        monkey.items = vec![]; // Monkeys never pass to themselves

        for item in item_adds {
            self.add_item(item.0, item.1);
        }
    }

    fn add_item(&mut self, monkey_id: usize, item: usize) {
        self.monkeys[monkey_id].items.push(item);
    }
}

fn make_monkey(monkey_str: &str) -> Monkey {
    let lines: Vec<&str> = monkey_str.trim().split("\n").collect();
    let mut item_str: Vec<&str> = lines[1].split(": ").collect();
    item_str = item_str[1].trim().split(", ").collect();
    let items: Vec<usize> = item_str.into_iter().map(|x| x.parse::<usize>().unwrap()).collect();

    let op_strs: Vec<&str> = lines[2].split("= ").collect();
    let op = op_from_str(op_strs[1]);

    let test_str: Vec<&str> = lines[3].trim().split(" ").collect();
    let test = test_str.last().unwrap().parse::<usize>().unwrap();

    let true_str: Vec<&str> = lines[4].trim().split(" ").collect();
    let true_target = true_str.last().unwrap().parse::<usize>().unwrap();
    let false_str: Vec<&str> = lines[5].trim().split(" ").collect();
    let false_target = false_str.last().unwrap().parse::<usize>().unwrap();

    Monkey { items: items, operation: op, test: test, targets: (true_target, false_target), inspections: 0 }
}

fn op_from_str(op_str: &str) -> Box<dyn Fn(usize) -> usize> {
    let pieces: Vec<&str> = op_str.split(" ").collect();
    let op = match pieces[1] {
        "*" => |x: usize, y: usize| -> usize { x * y },
        _ => |x: usize, y: usize| -> usize { x + y },
    };
    
    if pieces[2] == "old" {
        return Box::new(move |x: usize| -> usize { op(x, x) });
    } else {
        let n = pieces[2].parse::<usize>().unwrap();
        return Box::new(move |x: usize| -> usize { op(x, n) });
    }
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let monkey_chunks: Vec<&str> = file_data.trim().split("\n\n").collect();
    let mut monkeys = MonkeySet {monkeys: monkey_chunks.iter().map(|x| make_monkey(x)).collect()};

    for _ in 0..10000 {
        for i in 0..monkeys.monkeys.len() {
            monkeys.do_turn(i);
        }
    }

    let mut inspections: Vec<usize> = Vec::new();
    for monkey in monkeys.monkeys.iter() {
        inspections.push(monkey.inspections);
    }

    inspections.sort();
    inspections.reverse();

    println!("{}", inspections[0] * inspections[1]);
}
