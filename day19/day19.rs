#[derive(Debug)]
struct Cost {
    ore_cost: i64,
    clay_cost: i64,
    obsidian_cost: i64,
}

#[derive(Debug)]
struct Blueprint {
    id: i64,
    ore_bot_cost: Cost,
    clay_bot_cost: Cost,
    obs_bot_cost: Cost,
    geode_bot_cost: Cost,
}

#[derive(Clone, Copy, PartialEq, Hash, Debug)]
struct State {
    ore_bots: i64,
    clay_bots: i64,
    obs_bots: i64,
    geode_bots: i64,
    ore: i64,
    clay: i64,
    obsidian: i64,
    geodes: i64,
    time_remaining: i64,
}

fn neighbors(state: &State, blueprint: &Blueprint) -> Vec<State> {
    let mut neighbors: Vec<State> = Vec::new();
    if state.ore_bots * state.time_remaining >= blueprint.ore_bot_cost.ore_cost {
        let mut next_ore = state.clone();
        let tick = blueprint.ore_bot_cost.ore_cost - state.ore + 1;
        next_ore.ore_bots = state.ore_bots + 1;
        next_ore.ore = state.ore - blueprint.ore_bot_cost.ore_cost + tick * state.ore_bots;
        next_ore.clay = state.clay + tick * state.clay_bots;
        next_ore.obsidian = state.obsidian + tick * state.obs_bots;
        next_ore.geodes = state.geodes + tick * state.geode_bots;
        next_ore.time_remaining = state.time_remaining - tick;
        neighbors.push(next_ore);
    }
    if state.ore_bots * state.time_remaining >= blueprint.clay_bot_cost.ore_cost {
        let mut next_clay = state.clone();
        let tick = blueprint.clay_bot_cost.ore_cost - state.ore + 1;
        next_clay.clay_bots = state.clay_bots + 1;
        next_clay.ore = state.ore - blueprint.clay_bot_cost.ore_cost + tick * state.ore_bots;
        next_clay.clay = state.clay + tick * state.clay_bots;
        next_clay.obsidian = state.obsidian + tick * state.obs_bots;
        next_clay.geodes = state.geodes + tick * state.geode_bots;
        next_clay.time_remaining = state.time_remaining - tick;
        neighbors.push(next_clay);
    }
    if state.ore_bots * state.time_remaining >= blueprint.obs_bot_cost.ore_cost
        && state.clay_bots * state.time_remaining >= blueprint.obs_bot_cost.clay_cost
    {
        let mut next_obsidian = state.clone();
        let tick = std::cmp::max(
            blueprint.obs_bot_cost.ore_cost - state.ore,
            blueprint.obs_bot_cost.clay_cost - state.clay,
        ) + 1;
        next_obsidian.obs_bots = state.obs_bots + 1;
        next_obsidian.ore = state.ore - blueprint.obs_bot_cost.ore_cost + tick * state.ore_bots;
        next_obsidian.clay = state.clay - blueprint.obs_bot_cost.clay_cost + tick * state.clay_bots;
        next_obsidian.obsidian = state.obsidian + tick * state.obs_bots;
        next_obsidian.geodes = state.geodes + tick * state.geode_bots;
        next_obsidian.time_remaining = state.time_remaining - tick;
        neighbors.push(next_obsidian);
    }
    if state.ore_bots * state.time_remaining >= blueprint.geode_bot_cost.ore_cost
        && state.obs_bots * state.time_remaining >= blueprint.geode_bot_cost.obsidian_cost
    {
        let mut next_geode = state.clone();
        let tick = std::cmp::max(
            blueprint.geode_bot_cost.ore_cost - state.ore,
            blueprint.geode_bot_cost.obsidian_cost - state.obsidian,
        ) + 1;
        next_geode.geode_bots = state.geode_bots + 1;
        next_geode.ore = state.ore - blueprint.geode_bot_cost.ore_cost + tick * state.ore_bots;
        next_geode.obsidian = state.obsidian - blueprint.geode_bot_cost.obsidian_cost + tick * state.obs_bots;
        next_geode.clay = state.clay - blueprint.geode_bot_cost.clay_cost + tick * state.clay_bots;
        next_geode.geodes = state.geodes + tick * state.geode_bots;
        next_geode.time_remaining = state.time_remaining - tick;
        neighbors.push(next_geode);
    }

    if state.time_remaining > 0 && neighbors.is_empty() {
        let mut run_out_the_clock = state.clone();
        run_out_the_clock.ore = state.ore + state.time_remaining * state.ore_bots;
        run_out_the_clock.clay = state.clay + state.time_remaining * state.clay_bots;
        run_out_the_clock.obsidian = state.obsidian + state.time_remaining * state.obs_bots;
        run_out_the_clock.geodes = state.geodes + state.time_remaining * state.geode_bots;
    }
    return neighbors;
}

fn parse_line(line: &str) -> Blueprint {
    let words: Vec<&str> = line.split(" ").collect();
    let id_str = words[1].split(":").nth(0).unwrap();
    let id = id_str.parse().unwrap();
    let ore_bot_cost = Cost {
        ore_cost: words[6].parse().unwrap(),
        clay_cost: 0,
        obsidian_cost: 0,
    };
    let clay_bot_cost = Cost {
        ore_cost: words[12].parse().unwrap(),
        clay_cost: 0,
        obsidian_cost: 0,
    };
    let obsidian_bot_cost = Cost {
        ore_cost: words[18].parse().unwrap(),
        clay_cost: words[21].parse().unwrap(),
        obsidian_cost: 0,
    };
    let geode_bot_cost = Cost {
        ore_cost: words[27].parse().unwrap(),
        clay_cost: 0,
        obsidian_cost: words[30].parse().unwrap(),
    };
    Blueprint {
        id,
        ore_bot_cost,
        clay_bot_cost,
        obs_bot_cost: obsidian_bot_cost,
        geode_bot_cost,
    }
}

fn traverse_blueprint(state: &State, blueprint: &Blueprint) -> Vec<State> {
    if state.time_remaining == 0 {
        return vec![*state];
    } else if state.time_remaining < 0 {
        return vec![];
    }
    let mut nodes: Vec<State> = Vec::new();
    for neighbor in neighbors(state, blueprint).iter() {
        nodes.extend(traverse_blueprint(neighbor, blueprint));
    }

    return nodes;
}

fn main() {
    let raw_data = std::fs::read_to_string("test.txt").unwrap();
    let lines: Vec<&str> = raw_data.split("\n").filter(|x| !x.is_empty()).collect();
    let blueprints: Vec<Blueprint> = lines.iter().map(|x| parse_line(x)).collect();

    let init_state = State {
        ore_bots: 1,
        clay_bots: 0,
        obs_bots: 0,
        geode_bots: 0,
        ore: 0,
        clay: 0,
        obsidian: 0,
        geodes: 0,
        time_remaining: 24,
    };
    let bp1 = traverse_blueprint(&init_state, &blueprints[0]);
    let mut best = 0;
    for state in bp1.iter() {
        if state.geodes > best {
            println!("{:?}", state);
            best = state.geodes
        }
    }
}
