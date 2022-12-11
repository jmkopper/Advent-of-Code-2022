fn scenic_score(row: usize, col: usize, trees: &Vec<Vec<u32>>) -> u32 {
    let mut left_score = 1u32;
    let mut right_score = 1u32;
    let mut up_score = 1u32;
    let mut down_score = 1u32;

    let height = trees[row][col];

    for i in 1..col+1 {
        left_score = i as u32;
        if trees[row][col-i] >= height {
            break;
        }
    }

    for i in col+1..trees[0].len() {
        right_score = (i-col) as u32;
        if trees[row][i] >= height {
            break;
        }
    }

    for i in 1..row+1 {
        up_score = i as u32;
        if trees[row-i][col] >= height {
            break;
        }
    }

    for i in row+1..trees.len() {
        down_score = (i-row) as u32;
        if trees[i][col] >= height {
            break
        }
    }

    left_score * right_score * up_score * down_score
}


fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let row_strs: Vec<&str> = file_data.trim().split("\n").collect();
    let mut rows: Vec<Vec<u32>> = Vec::new();
    for row_str in row_strs[..row_strs.len()].iter() {
        let digits: Vec<&str> = row_str.trim().split("").collect();
        let digit_ints: Vec<u32> = digits[1..digits.len()-1].iter().map(|x| x.parse::<u32>().unwrap()).collect();
        rows.push(digit_ints);
    }

    let mut scores = vec![vec![0u32; rows[0].len()]; rows.len()];
    let mut max = 0u32;
    for (i, row) in rows.iter().enumerate() {
        for (j, col) in row.iter().enumerate() {
            let s = scenic_score(i, j, &rows);
            scores[i][j] = s;
            max = std::cmp::max(max, s);
        }
    }
    
    println!("{:?}", max);
}
