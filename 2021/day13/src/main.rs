use std::collections::HashSet;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
enum Fold {
    X(usize),
    Y(usize),
}

fn parse(content: &str) -> (HashSet<(usize, usize)>, Vec<Fold>) {
    let mut set = HashSet::default();
    let mut folds = vec![];
    let lines = content.lines();
    let mut read_points = true;
    for line in lines {
        if line.is_empty() {
            read_points = false;
        } else if read_points {
            let v: Vec<usize> = line
                .split(',')
                .map(|s| s.parse::<usize>().unwrap())
                .collect();
            set.insert((v[0], v[1]));
        } else {
            let v: Vec<&str> = line.split_whitespace().collect();
            let kv: Vec<&str> = v[2].split('=').collect();
            let i = kv[1].parse::<usize>().unwrap();
            if kv[0] == "y" {
                folds.push(Fold::Y(i));
            } else {
                folds.push(Fold::X(i));
            }
        }
    }
    (set, folds)
}

fn fold(set: HashSet<(usize, usize)>, f: &Fold) -> HashSet<(usize, usize)> {
    let mut remove: HashSet<(usize, usize)> = HashSet::new();
    let mut add: HashSet<(usize, usize)> = HashSet::new();
    for &(x, y) in &set {
        match f {
            Fold::Y(mid_y) => {
                if y > *mid_y {
                    remove.insert((x, y));
                    add.insert((x, mid_y * 2 - y));
                }
            }
            Fold::X(mid_x) => {
                if x > *mid_x {
                    remove.insert((x, y));
                    add.insert((mid_x * 2 - x, y));
                }
            }
        }
    }
    &(&set - &remove) | &add
}

fn day13a(content: &str) -> usize {
    let (set, folds) = parse(content);
    fold(set, &folds[0]).len()
}

fn day13b(content: &str) {
    let (set, folds) = parse(content);
    let result = folds.iter().fold(set, fold);

    let max_x = result
        .iter()
        .max_by(|(a, _), (b, _)| a.cmp(b))
        .map(|(x, _)| x)
        .unwrap();
    let max_y = result
        .iter()
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .map(|(_, y)| y)
        .unwrap();
    let mut display = vec![vec![false; *max_x + 1]; *max_y + 1];
    for (x, y) in result {
        display[y][x] = true;
    }
    for line in display {
        for v in line {
            print!("{}", if v { "X" } else { " " });
        }
        println!();
    }
}

fn main() {
    let content = include_str!("input13a.txt");
    println!("day13a: {}", day13a(content));
    println!("day13b:");
    day13b(content);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test13a.txt");
        let (set, folds) = parse(content);

        assert_eq!(18, set.len());
        assert_eq!(vec![Fold::Y(7), Fold::X(5)], folds);
    }

    #[test]
    fn test_fold() {
        let content = include_str!("test13a.txt");
        let (set, folds) = parse(content);
        let fold1 = fold(set, &folds[0]);
        assert_eq!(17, fold1.len());
        let fold2 = fold(fold1, &folds[1]);
        assert_eq!(16, fold2.len());
    }
}
