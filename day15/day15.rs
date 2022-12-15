// use std::collections::HashSet;

#[derive(Debug)]
struct Sensor {
    location: (i64, i64),
    nearest_beacon: (i64, i64),
    range: i64,
}

fn dist(a: (i64, i64), b: (i64, i64)) -> i64 {
    return (a.0 - b.0).abs() + (a.1 - b.1).abs()
}

// takes something of the form x=1234, y=456
fn parse_coord_str(coord_str: &str) -> (i64, i64) {
    let chunks: Vec<&str> = coord_str.split(", ").collect(); // ["x=123", "y=456"]
    return (chunks[0][2..].parse().unwrap(), chunks[1][2..].parse().unwrap())
}

fn parse_line(line: &str) -> Sensor {
    let chunks: Vec<&str> = line.trim().split(" at ").collect();
    let loc_str: Vec<&str> = chunks[1].split(":").collect();
    let location = parse_coord_str(&loc_str[0]);
    let nearest_beacon = parse_coord_str(&chunks[2]);
    let range = dist(location, nearest_beacon);
    Sensor { location, nearest_beacon, range }
}


fn count_in_row(sensors: &Vec<Sensor>, row: i64) -> i64 {
    let mut leftmost = std::i64::MAX;
    let mut rightmost = std::i64::MIN;

    for sensor in sensors.iter() {
        let row_radius = sensor.range - dist(sensor.location, (sensor.location.0, row));
        leftmost = std::cmp::min(leftmost, sensor.location.0-row_radius);
        rightmost = std::cmp::max(rightmost, sensor.location.0+row_radius);
    }

    let mut sum = 0;
    for i in leftmost..=rightmost {
        if sensors.iter().any(|x| dist((i, row), x.location) <= x.range) && sensors.iter().all(|x| (i,row) != x.nearest_beacon && (i,row) != x.location) {
            sum += 1;
        }
    }
    sum
}

fn find_col(sensors: &Vec<Sensor>, row: i64, min: i64, max: i64) -> Option<i64> {
    let mut segments: Vec<(i64, i64)> = Vec::new();
    for sensor in sensors.iter() {
        let row_radius = sensor.range - dist(sensor.location, (sensor.location.0, row));
        if row_radius >= 0 {
            segments.push((std::cmp::max(sensor.location.0 - row_radius, min), std::cmp::min(sensor.location.0 + row_radius, max)));
        }
    }

    segments.sort_by(|a, b| a.0.cmp(&b.0));

    if segments[0].0 > min {
        return Some(min);
    }

    let mut right = segments[0].1;
    for segment in segments.iter() {
        if segment.0 <= right {
            right = std::cmp::max(right, segment.1);
        } else {
            return Some(right + 1);
        }
    }

    if right < max {
        return Some(max);
    }

    None
}

fn tuning_frequency(sensors: &Vec<Sensor>, min: i64, max: i64) -> i64 {
    for row in min..=max {
        match find_col(sensors, row, min, max) {
            Some(x) => return x * 4000000 + row,
            None => {},
        }
    }

    return -1
}

fn main() {
    let raw_data = std::fs::read_to_string("input.txt").unwrap();
    let lines: Vec<&str> = raw_data.trim().split("\n").collect();
    let sensors: Vec<Sensor> = lines.iter().map(|x| parse_line(x)).collect();
    println!("part 1 {:?}", count_in_row(&sensors, 2000000));
    println!("part 2 {:?}", tuning_frequency(&sensors, 0, 4000000));
}
