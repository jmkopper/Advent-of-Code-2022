fn visible_from_start(trees: &Vec<u8>) -> Vec<u8> {
    let mut tallest = trees[0];
    let mut visible = vec![0u8; trees.len()];
    visible[0] = 1;
    for (i, tree) in trees[1..].iter().enumerate() {
        if tree > &tallest {
            tallest = *tree;
            visible[i+1] = 1;
        }
    }
    visible
}

fn update_visible_row(visible: &mut Vec<Vec<u8>>, new_viz: &mut Vec<u8>, row: usize) {
    for i in 0..new_viz.len() {
        visible[row][i] = std::cmp::max(new_viz[i], visible[row][i]);
    }
}

fn update_visible_col(visible: &mut Vec<Vec<u8>>, new_viz: &mut Vec<u8>, col: usize) {
    for i in 0..new_viz.len() {
        visible[i][col] = std::cmp::max(new_viz[i], visible[i][col]);
    }
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let row_strs: Vec<&str> = file_data.trim().split("\n").collect();
    let mut rows: Vec<Vec<u8>> = Vec::new();
    for row_str in row_strs[..row_strs.len()-1].iter() {
        let digits: Vec<&str> = row_str.trim().split("").collect();
        let digit_ints: Vec<u8> = digits[1..digits.len()-1].iter().map(|x| x.parse::<u8>().unwrap()).collect();
        rows.push(digit_ints);
    }

    let mut visible = vec![vec![0u8; rows.len()]; rows[0].len()];
    let tree_test: Vec<u8> = vec![1,2,3,2,1,4];
    println!("{:?}", visible_from_start(&tree_test));
}
