use std::collections::HashMap;

type Point = (usize, usize);
type Line = (Point, Point);

fn parse(content: &str) -> Vec<Line> {
    let mut v: Vec<Line> = vec![];
    for line in content.lines() {
        let l: Vec<usize> = line
            .split(" -> ")
            .flat_map(|pt| pt.split(","))
            .map(|s| s.parse().unwrap())
            .collect();
        let parsed: Line = ((l[0], l[1]), (l[2], l[3]));
        v.push(parsed);
    }
    v
}

fn day05a(content: &str) -> usize {
    let lines = parse(content);
    let filtered = lines
        .iter()
        .filter(|((x1, y1), (x2, y2))| x1 == x2 || y1 == y2);
    let mut count: HashMap<Point, usize> = HashMap::new();
    for &((x1, y1), (x2, y2)) in filtered {
        for x in x1.min(x2)..=x1.max(x2) {
            for y in y1.min(y2)..=y1.max(y2) {
                let pt = (x, y);
                if let Some(n) = count.get_mut(&pt) {
                    *n += 1;
                } else {
                    count.insert(pt, 1);
                }
            }
        }
    }
    count.iter().filter(|&(_, v)| *v > 1).count()
}

fn diag(((x1, y1), (x2, y2)): &Line) -> bool {
    let delta_x = *x2 as i32 - *x1 as i32;
    let delta_y = *y2 as i32 - *y1 as i32;
    delta_x.abs() == delta_y.abs()
}

fn day05b(content: &str) -> usize {
    let lines = parse(content);
    let filtered = lines
        .iter()
        .filter(|((x1, y1), (x2, y2))| x1 == x2 || y1 == y2);
    let mut count: HashMap<Point, usize> = HashMap::new();
    for &((x1, y1), (x2, y2)) in filtered {
        for x in x1.min(x2)..=x1.max(x2) {
            for y in y1.min(y2)..=y1.max(y2) {
                let pt = (x, y);
                if let Some(n) = count.get_mut(&pt) {
                    *n += 1;
                } else {
                    count.insert(pt, 1);
                }
            }
        }
    }
    let filtered = lines.iter().filter(|line| diag(line));
    for &((x1, y1), (x2, y2)) in filtered {
        let mut x = x1;
        let mut y = y1;
        while x != x2 && y != y2 {
            let pt = (x, y);
            if let Some(n) = count.get_mut(&pt) {
                *n += 1;
            } else {
                count.insert(pt, 1);
            }
            if x > x2 {
                x -= 1;
            } else {
                x += 1;
            }
            if y > y2 {
                y -= 1;
            } else {
                y += 1;
            }
        }
        let pt = (x, y);
        if let Some(n) = count.get_mut(&pt) {
            *n += 1;
        } else {
            count.insert(pt, 1);
        }
    }
    count.iter().filter(|&(_, v)| *v > 1).count()
}

fn main() {
    let content = include_str!("input05a.txt");
    println!("day05a: {}", day05a(content));
    println!("day05b: {}", day05b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test05a.txt");
        let lines = parse(content);
        assert_eq!(10, lines.len())
    }

    #[test]
    fn test_day05a() {
        let content = include_str!("test05a.txt");
        let count = day05a(content);
        assert_eq!(5, count)
    }

    #[test]
    fn test_diag() {
        assert!(!diag(&((1, 1), (1, 3))));
        assert!(diag(&((1, 1), (3, 3))));
        assert!(diag(&((9, 7), (7, 9))));
    }

    #[test]
    fn test_day05b() {
        let content = include_str!("test05a.txt");
        let count = day05b(content);
        assert_eq!(12, count);
        let content = include_str!("input05a.txt");
        let count = day05b(content);
        assert_eq!(19571, count)
    }
}
