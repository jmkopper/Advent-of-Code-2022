fn visible_from_start(trees: &Vec<u32>) -> Vec<u32> {
    let mut tallest = trees[0];
    let mut visible = vec![0u32; trees.len()];
    visible[0] = 1;
    for (i, tree) in trees[1..].iter().enumerate() {
        if tree > &tallest {
            tallest = *tree;
            visible[i+1] = 1;
        }
    }
    visible
}


fn update_visible_row(visible: &mut Vec<Vec<u32>>, new_viz: &Vec<u32>, row: usize) {
    for i in 0..new_viz.len() {
        visible[row][i] = std::cmp::max(new_viz[i], visible[row][i]);
    }
}

fn update_visible_col(visible: &mut Vec<Vec<u32>>, new_viz: &Vec<u32>, col: usize) {
    for i in 0..new_viz.len() {
        visible[i][col] = std::cmp::max(new_viz[i], visible[i][col]);
    }
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

    let mut visible = vec![vec![0u32; rows[0].len()]; rows.len()];

    for (i, row) in rows.iter().enumerate() {
        let mut new_viz = visible_from_start(row);
        update_visible_row(&mut visible, &new_viz, i);

        let mut reversed_row = row.to_vec();
        reversed_row.reverse();
        new_viz = visible_from_start(&reversed_row);
        new_viz.reverse();
        update_visible_row(&mut visible, &new_viz, i);
    }

    for i in 0..rows[0].len() {
        let mut the_col: Vec<u32> = Vec::new();
        for j in 0..rows.len() {
            the_col.push(rows[j][i]);
        }
        
        let mut new_viz = visible_from_start(&the_col);
        update_visible_col(&mut visible, &new_viz, i);

        the_col.reverse();
        new_viz = visible_from_start(&the_col);
        new_viz.reverse();
        update_visible_col(&mut visible, &new_viz, i);
    }

    let sums: Vec<u32> = visible.iter().map(|x| x.iter().sum()).collect();
    let total: u32 = sums.iter().sum();
    println!("{:?}", total);
}
