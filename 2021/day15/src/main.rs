use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;

type Grid = Vec<Vec<usize>>;
type Coord = (usize, usize);

fn parse(content: &str) -> Grid {
    content
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn adjacents(q: &HashSet<Coord>, (line, col): Coord) -> HashSet<Coord> {
    let mut result = HashSet::new();
    if line > 0 && q.contains(&(line - 1, col)) {
        result.insert((line - 1, col));
    }
    if col > 0 && q.contains(&(line, col - 1)) {
        result.insert((line, col - 1));
    }
    if q.contains(&(line + 1, col)) {
        result.insert((line + 1, col));
    }
    if q.contains(&(line, col + 1)) {
        result.insert((line, col + 1));
    }
    result
}

fn vertex_set(grid: &Grid) -> HashSet<Coord> {
    let mut q: HashSet<Coord> = HashSet::new();
    for (i, line) in grid.iter().enumerate() {
        for col in 0..line.len() {
            q.insert((i, col));
        }
    }
    q
}

/// src: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
fn dijkstra(grid: &Grid) -> Vec<Coord> {
    let vertices = vertex_set(grid);
    let mut q = BinaryHeap::<Reverse<(usize, Coord)>>::new();
    let mut dist: HashMap<Coord, usize> = vertices.iter().map(|k| (*k, usize::MAX)).collect();
    let mut prev: HashMap<Coord, Coord> = HashMap::new();
    let target = (grid.len() - 1, grid[0].len() - 1);

    dist.insert((0, 0), 0);
    q.push(Reverse((0, (0, 0))));

    while let Some(Reverse((cost, u))) = q.pop() {
        if u == target {
            break;
        }
        if cost > dist[&u] {
            continue;
        }
        for v @ (line, col) in adjacents(&vertices, u) {
            let alt = dist[&u] + grid[line][col];
            if alt < dist[&v] {
                q.push(Reverse((alt, v)));
                dist.insert(v, alt);
                prev.insert(v, u);
            }
        }
    }

    let mut s = vec![target];
    let mut u = target;
    while let Some(v) = prev.get(&u) {
        s.push(*v);
        u = *v;
    }
    s.pop();
    s
}

fn risk(v: usize, x: usize, y: usize) -> usize {
    let s = v + x + y;
    if s > 9 {
        s - 9
    } else {
        s
    }
}

fn full_map(grid: Grid) -> Grid {
    let mut result = vec![];
    for y in 0..5 {
        for line in &grid {
            let mut big_line = vec![];
            for x in 0..5 {
                big_line.extend(line.iter().map(|&v| risk(v, x, y)));
            }
            result.push(big_line);
        }
    }

    result
}

fn day15a(content: &str) -> usize {
    let grid = parse(content);
    dijkstra(&grid)
        .iter()
        .map(|&(line, col)| grid[line][col])
        .sum()
}

fn day15b(content: &str) -> usize {
    let grid = full_map(parse(content));
    dijkstra(&grid)
        .iter()
        .map(|&(line, col)| grid[line][col])
        .sum()
}

fn main() {
    let content = include_str!("input15a.txt");
    println!("day15a: {}", day15a(content));
    println!("day15b: {}", day15b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test15a.txt");
        let grid = parse(content);
        assert_eq!(10, grid.len());

        for line in grid {
            assert_eq!(10, line.len());
        }
    }

    #[test]
    fn test_adjacent() {
        let content = include_str!("test15a.txt");
        let grid = parse(content);
        let q = vertex_set(&grid);

        assert_eq!(2, adjacents(&q, (0, 0)).len());
        assert_eq!(3, adjacents(&q, (0, 1)).len());
        assert_eq!(4, adjacents(&q, (1, 1)).len());
        assert_eq!(2, adjacents(&q, (9, 9)).len());
    }

    #[test]
    fn test_djikstra() {
        let content = include_str!("test15a.txt");
        let grid = parse(content);
        assert_eq!(18, dijkstra(&grid).len());
    }

    #[test]
    fn test_day15a() {
        let content = include_str!("test15a.txt");
        assert_eq!(40, day15a(content))
    }

    #[test]
    fn test_risk() {
        assert_eq!(8, risk(8, 0, 0));
        assert_eq!(9, risk(8, 1, 0));
        assert_eq!(9, risk(8, 0, 1));
        assert_eq!(1, risk(8, 2, 0));
        assert_eq!(1, risk(8, 1, 1));
        assert_eq!(1, risk(8, 0, 2));
    }

    #[test]
    fn test_full_map() {
        let content = include_str!("test15a.txt");
        let grid = full_map(parse(content));

        assert_eq!(50, grid.len());
        for line in grid {
            assert_eq!(50, line.len());
        }
    }

    #[test]
    fn test_day15b() {
        let content = include_str!("test15a.txt");
        assert_eq!(315, day15b(content));
    }
}
