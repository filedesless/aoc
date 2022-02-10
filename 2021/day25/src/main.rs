use std::collections::HashSet;

#[derive(Debug)]
struct Grid {
    w: usize,
    h: usize,
    east: HashSet<(usize, usize)>,
    south: HashSet<(usize, usize)>,
}

impl PartialEq for Grid {
    fn eq(&self, other: &Self) -> bool {
        if self.w != other.w {
            return false;
        }
        if self.h != other.h {
            return false;
        }
        if !self.east.is_subset(&other.east) || !self.east.is_superset(&other.east) {
            return false;
        }
        if !self.south.is_subset(&other.south) || !self.south.is_superset(&other.south) {
            return false;
        }

        true
    }
}

impl Grid {
    /// returns wether the sets changed in any way
    fn step(&mut self) -> bool {
        let mut result = false;
        let mut add = HashSet::<(usize, usize)>::new();
        let mut del = HashSet::<(usize, usize)>::new();
        for &(lin, col) in self.east.iter() {
            let attempt = (lin, (col + 1) % self.w);
            if !(self.east.contains(&attempt) || self.south.contains(&attempt)) {
                result = true;
                add.insert(attempt);
                del.insert((lin, col));
            }
        }
        self.east.extend(add.drain());
        self.east = &self.east - &del;
        del.clear();
        for &(lin, col) in self.south.iter() {
            let attempt = ((lin + 1) % self.h, col);
            if !(self.east.contains(&attempt) || self.south.contains(&attempt)) {
                result = true;
                add.insert(attempt);
                del.insert((lin, col));
            }
        }
        self.south.extend(add);
        self.south = &self.south - &del;
        result
    }
}

fn parse(content: &str) -> Grid {
    let mut south = HashSet::<(usize, usize)>::new();
    let mut east = HashSet::<(usize, usize)>::new();
    let h = content.lines().count();
    let w = content.lines().next().unwrap().len();
    for (lin, line) in content.lines().enumerate() {
        for (col, c) in line.chars().enumerate() {
            if c == '>' {
                east.insert((lin, col));
            } else if c == 'v' {
                south.insert((lin, col));
            }
        }
    }

    Grid { w, h, south, east }
}

fn day25a(content: &str) -> usize {
    let mut grid = parse(content);
    for i in 1.. {
        if !grid.step() {
            return i;
        }
    }
    0
}

fn main() {
    let content = include_str!("input25.txt");
    println!("day25a: {}", day25a(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test25a.txt");
        let grid = parse(content);
        assert_eq!(10, grid.w);
        assert_eq!(9, grid.h);
    }

    #[test]
    fn test_step() {
        let mut grid1 = parse("...>>>>>...");
        grid1.step();
        let mut grid2 = parse("...>>>>.>..");
        assert_eq!(grid2, grid1);
        grid2.step();
        let grid3 = parse("...>>>.>.>.");
        assert_eq!(grid3, grid2);

        let mut step0 = parse(include_str!("step0.txt"));
        assert_eq!(7, step0.w);
        assert_eq!(7, step0.h);
        let mut step1 = parse(include_str!("step1.txt"));
        step0.step();
        assert_eq!(step1, step0);
        let step2 = parse(include_str!("step2.txt"));
        step1.step();
        assert_eq!(step2, step1);
    }

    #[test]
    fn test_day25a() {
        let content = include_str!("test25a.txt");
        assert_eq!(58, day25a(content));
    }
}
