fn parse_command_blocks(data: &str) -> Vec<&str> {
    let resp: Vec<&str> = data.split("$ ").collect();
    resp[1..].to_vec()
}


#[derive(Debug)]
struct DirectoryNode {
    name: String,
    parent: Option<usize>,
    children: Option<Vec<usize>>,
    files: Option<Vec<usize>>,
    id: usize,
}

impl DirectoryNode {
    fn size(&self) -> usize {
        match &self.files {
            None => 0,
            Some(x) => x.iter().sum(),
        }
    }
}

#[derive(Debug)]
struct FileTree {
    cwd: usize,
    nodes: Vec<DirectoryNode>,
}

impl FileTree {
    fn new_child(&mut self, parent: usize, child_name: String) {
        let new_idx = self.nodes.len();
        self.nodes.push(DirectoryNode {
            name: child_name,
            parent: Some(parent),
            children: None,
            files: None,
            id: new_idx,
        });

        match self.nodes[self.cwd].children {
            Some(ref mut y) => y.push(new_idx),
            None => self.nodes[self.cwd].children = Some(vec![new_idx]),
        };
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Command {
    CD,
    LS,
}

fn parse_command_from_str(command_str: &str) -> Option<Command> {
    let split_line: Vec<&str> = command_str.split(" ").collect();
    let command_str = split_line[0];
    match command_str {
        "cd" => Some(Command::CD),
        "ls" => Some(Command::LS),
        _ => None,
    }
}

fn run_command(command: Command, tree: &mut FileTree, command_block: &Vec<&str>) {
    match command {
        Command::CD => run_cd(tree, command_block),
        Command::LS => run_ls(tree, command_block),
    }
}

fn run_cd(tree: &mut FileTree, command_block: &Vec<&str>) {
    let cmd_line = command_block[0];
    let line_split: Vec<&str> = cmd_line.split(" ").collect();
    let new_dir = line_split[1];

    if new_dir == "/" {
        tree.cwd = 0;
    } else if new_dir == ".." {
        tree.cwd = tree.nodes[tree.cwd].parent.unwrap();
    } else {
        let cwd_children = &tree.nodes[tree.cwd].children;
        for child in cwd_children.iter().flatten() {
            if tree.nodes[*child].name == new_dir {
                tree.cwd = tree.nodes[*child].id;
            }
        }
    }
}

fn run_ls(tree: &mut FileTree, command_block: &Vec<&str>) {
    let command_lines = &command_block[1..];
    let mut files: Vec<usize> = Vec::new();
    for command_line in command_lines.iter() {
        let line: Vec<&str> = command_line.split(" ").collect();

        if line[0] == "dir" {
            let cwd_children = &tree.nodes[tree.cwd].children;
            let new_dir_name = line[1];
            let mut already_seen = false;
            for child in cwd_children.iter().flatten() {
                if tree.nodes[*child].name == new_dir_name {
                    already_seen = true;
                }
            }

            if !already_seen {
                tree.new_child(tree.cwd, new_dir_name.to_string());
            }
        } else {
            let file_size = line[0].parse::<usize>().unwrap();
            files.push(file_size);
        }
    }

    if files.len() > 0 {
        tree.nodes[tree.cwd].files = Some(files);
    }
}

fn dir_size(tree: &FileTree, root: usize) -> usize {
    let mut size: usize = tree.nodes[root].size();
    for child in tree.nodes[root].children.iter().flatten() {
        size += dir_size(tree, *child);
    }
    size
}

fn main() {
    let file_data = std::fs::read_to_string("input.txt").unwrap();
    let command_blocks = parse_command_blocks(&file_data);
    let root = DirectoryNode {
        name: "/".to_string(),
        parent: None,
        children: Some(Vec::new()),
        files: Some(Vec::new()),
        id: 0,
    };
    let mut tree = FileTree {
        cwd: 0,
        nodes: vec![root],
    };
    for block in command_blocks.iter() {
        let command_block: Vec<&str> = block.trim().split("\n").collect();
        let command = parse_command_from_str(&command_block[0]).unwrap();
        run_command(command, &mut tree, &command_block);
    }

    let total_sizes: Vec<usize> = tree.nodes.iter().map(|x| dir_size(&tree, x.id)).collect();
    let current_space = 70000000 - dir_size(&tree, 0);
    let req_space = 30000000;
    let del_options = total_sizes
        .iter()
        .filter(|x| **x + current_space >= req_space);
    println!("{:?}", del_options.min().unwrap());
}
