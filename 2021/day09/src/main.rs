use std::collections::HashSet;
use std::collections::VecDeque;

type Grid = Vec<Vec<usize>>;
type Position = (usize, usize);

fn parse(content: &str) -> Grid {
    content
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| String::from(c).parse::<usize>().unwrap())
                .collect()
        })
        .collect()
}

fn adjacents(grid: &Grid, pos: Position) -> HashSet<Position> {
    let (linum, col) = pos;
    let mut result = HashSet::new();
    if grid
        .get(linum.wrapping_sub(1))
        .and_then(|line| line.get(col))
        .is_some()
    {
        result.insert((linum.wrapping_sub(1), col));
    }
    if grid.get(linum + 1).and_then(|line| line.get(col)).is_some() {
        result.insert((linum + 1, col));
    }
    if grid
        .get(linum)
        .and_then(|line| line.get(col.wrapping_sub(1)))
        .is_some()
    {
        result.insert((linum, col.wrapping_sub(1)));
    }
    if grid.get(linum).and_then(|line| line.get(col + 1)).is_some() {
        result.insert((linum, col + 1));
    }

    result
}

fn low(grid: &Grid, pos: Position) -> bool {
    let (linum, col) = pos;
    if let Some(current) = grid.get(linum).and_then(|line| line.get(col)) {
        return adjacents(grid, pos).iter().all(|&(linum, col)| {
            grid.get(linum)
                .and_then(|line| line.get(col))
                .map(|adj| adj >= current)
                .unwrap()
        });
    }
    true
}

/// returns coordinates part of a basin around given position, assumed to be a low point
fn bassin(grid: &Grid, pos: Position) -> HashSet<Position> {
    let mut result = HashSet::from_iter(vec![pos]);
    let mut visit: VecDeque<Position> = VecDeque::from_iter(vec![pos]);

    while let Some((linum, col)) = visit.pop_front() {
        for (other_linum, other_col) in adjacents(grid, (linum, col)) {
            if let Some(current) = grid.get(linum).and_then(|line| line.get(col)) {
                if let Some(other) = grid.get(other_linum).and_then(|line| line.get(other_col)) {
                    if *other != 9 && other > current {
                        visit.push_back((other_linum, other_col));
                        result.insert((other_linum, other_col));
                    }
                }
            }
        }
    }

    result
}

fn day09a(content: &str) -> usize {
    let grid = parse(content);
    let mut risk_level = 0;
    for (linum, line) in grid.iter().enumerate() {
        for (col, value) in line.iter().enumerate() {
            if low(&grid, (linum, col)) {
                risk_level += value + 1;
            }
        }
    }
    risk_level
}

fn day09b(content: &str) -> usize {
    let grid = parse(content);
    let mut numbers = vec![];
    for (linum, line) in grid.iter().enumerate() {
        for (col, _) in line.iter().enumerate() {
            if low(&grid, (linum, col)) {
                numbers.push(bassin(&grid, (linum, col)).len());
            }
        }
    }
    numbers.sort();
    numbers.reverse();

    numbers.iter().take(3).product()
}

fn main() {
    let content = include_str!("input09a.txt");
    println!("day09a: {}", day09a(content));
    println!("day09b: {}", day09b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test09a.txt");
        let v = parse(content);
        assert_eq!(5, v.len());
        for line in v {
            assert_eq!(10, line.len());
        }
    }

    #[test]
    fn test_low() {
        let content = include_str!("test09a.txt");
        let grid = parse(content);
        assert!(!low(&grid, (0, 0)));
        assert!(low(&grid, (0, 1)));
        assert!(low(&grid, (0, 9)));
        assert!(low(&grid, (2, 2)));
        assert!(low(&grid, (4, 6)));
    }

    #[test]
    fn test_day09a() {
        let content = include_str!("test09a.txt");
        assert_eq!(15, day09a(content));
    }

    #[test]
    fn test_bassin() {
        let content = include_str!("test09a.txt");
        let grid = parse(content);
        assert_eq!(
            HashSet::from_iter(vec![(0, 0), (0, 1), (1, 0)]),
            bassin(&grid, (0, 1))
        );
        assert_eq!(9, bassin(&grid, (0, 9)).len());
        assert_eq!(14, bassin(&grid, (2, 2)).len());
        assert_eq!(9, bassin(&grid, (4, 6)).len());
    }

    #[test]
    fn test_day09b() {
        let content = include_str!("test09a.txt");
        assert_eq!(1134, day09b(content));
    }
}
