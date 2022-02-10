use std::io::Error;

fn day02a(content: &str) -> usize {
    let mut depth = 0;
    let mut horizontal = 0;
    for line in content.lines() {
        let mut arr = line.split_whitespace();
        let command = arr.next().unwrap();
        let amount: usize = arr.next().unwrap().parse().unwrap();
        if command == "forward" {
            horizontal += amount;
        } else if command == "up" {
            depth -= amount;
        } else if command == "down" {
            depth += amount;
        }
    }

    depth * horizontal
}

fn day02b(content: &str) -> usize {
    let mut depth = 0;
    let mut horizontal = 0;
    let mut aim = 0;
    for line in content.lines() {
        let mut arr = line.split_whitespace();
        let command = arr.next().unwrap();
        let amount: usize = arr.next().unwrap().parse().unwrap();
        if command == "forward" {
            horizontal += amount;
            depth += aim * amount;
        } else if command == "up" {
            aim -= amount;
        } else if command == "down" {
            aim += amount;
        }
    }

    depth * horizontal
}

fn main() -> Result<(), Error> {
    let content = include_str!("input02a.txt");
    println!("day02a: {}", day02a(&content));
    println!("day02b: {}", day02b(&content));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day02a() {
        let content = include_str!("test02a.txt");
        assert_eq!(150, day02a(&content));
    }

    #[test]
    fn test_day02b() {
        let content = include_str!("test02a.txt");
        assert_eq!(900, day02b(&content));
    }
}
