use std::collections::HashSet;

#[derive(Debug, Clone)]
struct Board {
    lines: Vec<Vec<(u32, bool)>>,
}

impl Board {
    fn from_vec(v: Vec<Vec<(u32, bool)>>) -> Self {
        Self { lines: v }
    }

    pub fn mark(&mut self, number: u32) {
        for line in &mut self.lines {
            for t in line {
                if t.0 == number {
                    t.1 = true;
                }
            }
        }
    }

    pub fn solved(&self) -> bool {
        // check horizontal lines
        for line in &self.lines {
            if line.iter().all(|&(_, b)| b) {
                return true;
            }
        }

        // check vertical lines
        for col in 0..self.lines[0].len() {
            if (0..self.lines.len()).all(|linum| self.lines[linum][col].1) {
                return true;
            }
        }

        false
    }

    pub fn score(&self, number: u32) -> u32 {
        let mut sum = 0;
        for line in &self.lines {
            for (n, b) in line {
                if !b {
                    sum += n;
                }
            }
        }
        number * sum
    }
}

fn parse(content: &str) -> (Vec<u32>, Vec<Board>) {
    let mut lines = content.lines();
    let first_line = lines.next().unwrap();
    let numbers: Vec<u32> = first_line.split(",").map(|i| i.parse().unwrap()).collect();
    let _ = lines.next();
    let mut boards: Vec<Board> = vec![];
    let mut board: Vec<Vec<(u32, bool)>> = vec![];
    for line in lines {
        if line.is_empty() {
            boards.push(Board::from_vec(board));
            board = vec![];
            continue;
        }
        let v: Vec<(u32, bool)> = line
            .split_ascii_whitespace()
            .map(|n| (n.parse().unwrap(), false))
            .collect();
        board.push(v);
    }
    boards.push(Board::from_vec(board));
    (numbers, boards)
}

fn day04a(content: &str) -> u32 {
    let (numbers, mut boards) = parse(content);
    let (number, board) = numbers
        .iter()
        .find_map(|&number| {
            for board in &mut boards {
                board.mark(number);
                if board.solved() {
                    return Some((number, board.clone()));
                }
            }
            None
        })
        .unwrap();
    board.score(number)
}

fn day04b(content: &str) -> u32 {
    let (numbers, mut boards) = parse(content);
    let mut winset: HashSet<usize> = HashSet::new();
    for number in numbers {
        for i in 0..boards.len() {
            boards[i].mark(number);
            if !winset.contains(&i) && boards[i].solved() {
                winset.insert(i);
                if winset.len() == boards.len() {
                    return boards[i].score(number);
                }
            }
        }
    }
    0
}

fn main() {
    let content = include_str!("input04a.txt");
    println!("day04a: {}", day04a(content));
    println!("day04b: {}", day04b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test04a.txt");
        let (numbers, board) = parse(content);
        assert_eq!(27, numbers.len());
        assert_eq!(3, board.len());
    }

    #[test]
    fn test_day04a() {
        let content = include_str!("test04a.txt");
        let score = day04a(content);
        assert_eq!(4512, score);
    }

    #[test]
    fn test_day04b() {
        let content = include_str!("test04a.txt");
        let score = day04b(content);
        assert_eq!(1924, score);
    }
}
