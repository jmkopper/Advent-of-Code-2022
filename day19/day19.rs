use std::{
    collections::{HashSet, VecDeque, HashMap},
    hash::Hash,
};

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
    max_ore: i64,
    max_clay: i64,
    max_obs: i64
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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
    if state.time_remaining <= 0 {
        return vec![];
    }

    if state.ore >= blueprint.geode_bot_cost.ore_cost
        && state.obsidian >= blueprint.geode_bot_cost.obsidian_cost
    {
        let mut buy_geode_bot = state.clone();
        buy_geode_bot.geode_bots = state.geode_bots + 1;
        buy_geode_bot.ore = state.ore - blueprint.geode_bot_cost.ore_cost + state.ore_bots;
        buy_geode_bot.obsidian =
            state.obsidian - blueprint.geode_bot_cost.obsidian_cost + state.obs_bots;
        buy_geode_bot.clay = state.clay + state.clay_bots;
        buy_geode_bot.geodes = state.geodes + state.geode_bots;
        buy_geode_bot.time_remaining -= 1;
        return vec![buy_geode_bot];
    }

    let mut neighbors: Vec<State> = Vec::new();
    if state.ore >= blueprint.ore_bot_cost.ore_cost  && state.ore_bots < blueprint.max_ore {
        let mut buy_ore_bot = state.clone();
        buy_ore_bot.ore_bots = state.ore_bots + 1;
        buy_ore_bot.ore = state.ore - blueprint.ore_bot_cost.ore_cost + state.ore_bots;
        buy_ore_bot.clay = state.clay + state.clay_bots;
        buy_ore_bot.obsidian = state.obsidian + state.obs_bots;
        buy_ore_bot.geodes = state.geodes + state.geode_bots;
        buy_ore_bot.time_remaining -= 1;
        neighbors.push(buy_ore_bot);
    }
    if state.ore >= blueprint.clay_bot_cost.ore_cost && state.clay_bots < blueprint.max_clay {
        let mut buy_clay_bot = state.clone();
        buy_clay_bot.clay_bots = state.clay_bots + 1;
        buy_clay_bot.ore = state.ore - blueprint.clay_bot_cost.ore_cost + state.ore_bots;
        buy_clay_bot.clay = state.clay + state.clay_bots;
        buy_clay_bot.obsidian = state.obsidian + state.obs_bots;
        buy_clay_bot.geodes = state.geodes + state.geode_bots;
        buy_clay_bot.time_remaining -= 1;
        neighbors.push(buy_clay_bot);
    }
    if state.ore >= blueprint.obs_bot_cost.ore_cost
        && state.clay >= blueprint.obs_bot_cost.clay_cost && state.obs_bots < blueprint.max_obs
    {
        let mut buy_obs_bot = state.clone();
        buy_obs_bot.obs_bots = state.obs_bots + 1;
        buy_obs_bot.ore = state.ore - blueprint.obs_bot_cost.ore_cost + state.ore_bots;
        buy_obs_bot.clay = state.clay - blueprint.obs_bot_cost.clay_cost + state.clay_bots;
        buy_obs_bot.obsidian = state.obsidian + state.obs_bots;
        buy_obs_bot.geodes = state.geodes + state.geode_bots;
        buy_obs_bot.time_remaining -= 1;
        neighbors.push(buy_obs_bot);
    }

    let mut wait = state.clone();
    wait.ore = state.ore + state.ore_bots;
    wait.clay = state.clay + state.clay_bots;
    wait.obsidian = state.obsidian + state.obs_bots;
    wait.geodes = state.geodes + state.geode_bots;
    wait.time_remaining -= 1;
    neighbors.push(wait);

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
    let &max_ore = vec![ore_bot_cost.ore_cost, clay_bot_cost.ore_cost, obsidian_bot_cost.ore_cost, geode_bot_cost.ore_cost].iter().max().unwrap();
    let max_clay = obsidian_bot_cost.clay_cost;
    let max_obs = geode_bot_cost.obsidian_cost;
    Blueprint {
        id,
        ore_bot_cost,
        clay_bot_cost,
        obs_bot_cost: obsidian_bot_cost,
        geode_bot_cost,
        max_ore,
        max_clay,
        max_obs,
    }
}

fn bfs_blueprint(state: &State, blueprint: &Blueprint) -> i64 {
    let mut queue: VecDeque<State> = VecDeque::new();
    let mut parents: HashSet<State> = HashSet::new();
    let mut cache: HashMap<i64, i64> = HashMap::new();
    for i in 0..=state.time_remaining {
        cache.insert(i, 0);
    }

    queue.push_back(*state);
    while queue.len() > 0 && state.time_remaining >= 0 {
        let v = queue.pop_front().unwrap();
        if v.geodes < *cache.get(&v.time_remaining).unwrap() {
            continue;
        }

        cache.insert(v.time_remaining, v.geodes);

        for n in neighbors(&v, blueprint).into_iter() {
            if !parents.contains(&n) {
                parents.insert(n);
                queue.push_back(n);
            }
        }
    }

    *cache.get(&0).unwrap()
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
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

    let mut init_state_pt2 = init_state.clone();
    init_state_pt2.time_remaining = 32;

    let quality_levels: Vec<i64> = blueprints.iter().map(|b| bfs_blueprint(&init_state, b)*b.id).collect();
    println!("part 1: {:?}", quality_levels.iter().sum::<i64>());

    let uneaten_blueprints: Vec<&Blueprint> = blueprints.iter().take(3).collect();
    let geodes: Vec<i64> = uneaten_blueprints.iter().map(|b| bfs_blueprint(&init_state_pt2, b)).collect();
    println!("part 2: {:?}", geodes[0]*geodes[1]*geodes[2]);
}
