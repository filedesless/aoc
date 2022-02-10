use std::collections::VecDeque;

type Octopus = (usize, bool);
type Grid = Vec<Vec<Octopus>>;
type Position = (usize, usize);

const GRID_HEIGHT: usize = 10;
const GRID_WIDTH: usize = 10;

fn parse(content: &str) -> Grid {
    content
        .lines()
        .map(|line| {
            (line
                .chars()
                .map(|c| (c.to_digit(10).unwrap() as usize, false)))
            .collect()
        })
        .collect()
}

/// returns all valid positions around a given point
fn adjacents(pos: Position) -> VecDeque<Position> {
    let (linum, col) = pos;
    let mut result = VecDeque::new();
    for x in linum.saturating_sub(1)..GRID_WIDTH.min(linum + 2) {
        for y in col.saturating_sub(1)..GRID_HEIGHT.min(col + 2) {
            if (x, y) != pos {
                result.push_back((x, y));
            }
        }
    }
    result
}

fn simulate_step(grid: &mut Grid) -> usize {
    let mut flashes = 0;
    let mut visiting: VecDeque<Position> = VecDeque::new();
    for linum in 0..grid.len() {
        for col in 0..grid[linum].len() {
            let (level, flashed) = &mut grid[linum][col];
            // First, the energy level of each octopus increases by 1
            *level += 1;
            // Then, any octopus with an energy level greater than 9 flashes
            if *level > 9 {
                *flashed = true;
                visiting.append(&mut adjacents((linum, col)));
            }
        }
    }

    // This increases the energy level of all adjacent octopuses by 1, including octopuses that are diagonally adjacent.
    while let Some((linum, col)) = visiting.pop_front() {
        let octo = grid.get_mut(linum).and_then(|line| line.get_mut(col));
        if let Some((level, flashed)) = octo {
            *level += 1;
            // If this causes an octopus to have an energy level greater than 9, it also flashes
            // An octopus can only flash at most once per step
            if *level > 9 && !*flashed {
                *flashed = true;
                // This process continues as long as new octopuses keep having their energy level increased beyond 9
                visiting.append(&mut adjacents((linum, col)));
            }
        }
    }

    // Finally, any octopus that flashed during this step has its energy level set to 0, as it used all of its energy to flash.
    for line in grid.iter_mut() {
        for (level, flashed) in line.iter_mut() {
            if *flashed {
                *level = 0;
                *flashed = false;
                flashes += 1;
            }
        }
    }
    flashes
}

fn simulate_n(mut grid: Grid, steps: usize) -> usize {
    let mut flashes = 0;
    for _step in 0..steps {
        flashes += simulate_step(&mut grid);
        // println!("After step {}:\n{:?}", _step + 1, grid);
    }
    flashes
}

fn day11a(grid: Grid) -> usize {
    simulate_n(grid, 100)
}

fn day11b(mut grid: Grid) -> usize {
    for step in 1.. {
        if simulate_step(&mut grid) == 100 {
            return step;
        }
    }
    unreachable!();
}

fn main() {
    let content = include_str!("input11a.txt");
    let grid = parse(content);
    println!("day11a: {}", day11a(grid.clone()));
    println!("day11b: {}", day11b(grid.clone()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test11a.txt");
        let grid = parse(content);
        assert_eq!(10, grid.len());

        for line in grid {
            assert_eq!(10, line.len());
            for (level, flashed) in line {
                assert!(level < 10);
                assert!(!flashed);
            }
        }
    }

    #[test]
    fn test_adjacents() {
        assert_eq!(3, adjacents((0, 0)).len());
        assert_eq!(5, adjacents((0, 1)).len());
        assert_eq!(3, adjacents((9, 9)).len());
        assert_eq!(5, adjacents((9, 8)).len());
        assert_eq!(8, adjacents((1, 1)).len());
    }

    #[test]
    fn test_simulate_n() {
        let content = include_str!("test11a.txt");
        let grid = parse(content);
        assert_eq!(0, simulate_n(grid.clone(), 1));
        assert_eq!(35, simulate_n(grid.clone(), 2));
        assert_eq!(204, simulate_n(grid.clone(), 10));
        assert_eq!(1656, simulate_n(grid.clone(), 100));
    }

    #[test]
    fn test_day11b() {
        let content = include_str!("test11a.txt");
        let grid = parse(content);
        assert_eq!(195, day11b(grid));
    }
}
