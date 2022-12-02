#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum RPSChoice {
    Rock,
    Paper,
    Scissors,
}

impl RPSChoice {
    fn beats(&self) -> RPSChoice {
        match self {
            RPSChoice::Rock => RPSChoice::Scissors,
            RPSChoice::Paper => RPSChoice::Rock,
            RPSChoice::Scissors => RPSChoice::Paper,
        }
    }

    fn loses_to(&self) -> RPSChoice {
        match self {
            RPSChoice::Rock => RPSChoice::Paper,
            RPSChoice::Paper => RPSChoice::Scissors,
            RPSChoice::Scissors => RPSChoice::Rock,
        }
    }

    fn score(&self) -> i32 {
        match self {
            RPSChoice::Rock => 1,
            RPSChoice::Paper => 2,
            RPSChoice::Scissors => 3,
        }
    }
}

#[derive(Debug)]
enum RPSOutcome {
    FirstWins,
    SecondWins,
    Draw,
}

impl RPSOutcome {
    fn score(&self) -> i32 {
        match self {
            RPSOutcome::Draw => 3,
            RPSOutcome::FirstWins => 0,
            RPSOutcome::SecondWins => 6,
        }
    }
}

fn str_to_rpschoice(s: &str) -> Option<RPSChoice> {
    match s {
        "A" => Some(RPSChoice::Rock),
        "B" => Some(RPSChoice::Paper),
        "C" => Some(RPSChoice::Scissors),
        _ => None,
    }
}

fn str_to_rpsoutcome(s: &str) -> Option<RPSOutcome> {
    match s {
        "X" => Some(RPSOutcome::FirstWins),
        "Y" => Some(RPSOutcome::Draw),
        "Z" => Some(RPSOutcome::SecondWins),
        _ => None,
    }
}

fn rps_cmp(pair: &Vec<RPSChoice>) -> RPSOutcome {
    if pair[0].beats() == pair[1] {
        return RPSOutcome::FirstWins;
    }
    if pair[1].beats() == pair[0] {
        return RPSOutcome::SecondWins;
    }
    return RPSOutcome::Draw;
}

fn get_pair_from_outcome(elf_choice: RPSChoice, desired_outcome: RPSOutcome) -> Vec<RPSChoice> {
    let choice = match desired_outcome {
        RPSOutcome::Draw => elf_choice,
        RPSOutcome::FirstWins => elf_choice.beats(),
        RPSOutcome::SecondWins => elf_choice.loses_to(),
    };
    return vec![elf_choice, choice];
}

fn score_pair(pair: &Vec<RPSChoice>) -> i32 {
    rps_cmp(pair).score() + pair[1].score()
}

fn main() -> std::io::Result<()> {
    let file_contents = std::fs::read_to_string("./input.txt")?;
    let file_content_pairs: Vec<Vec<&str>> = file_contents
        .trim()
        .split("\n")
        .map(|line| line.split(" ").collect())
        .collect();
    let pairs: Vec<Vec<RPSChoice>> = file_content_pairs
        .iter()
        .map(|pair| {
            get_pair_from_outcome(
                str_to_rpschoice(pair[0]).unwrap(),
                str_to_rpsoutcome(pair[1]).unwrap(),
            )
        })
        .collect();
    let score: i32 = pairs.iter().map(score_pair).sum();
    println!("{:?}", score);
    Ok(())
}
