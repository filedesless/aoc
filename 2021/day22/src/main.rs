use std::ops::Range;

#[derive(Clone, Eq, PartialEq, Debug)]
struct Cuboid {
    xs: Range<isize>,
    ys: Range<isize>,
    zs: Range<isize>,
    on: bool,
}

impl Cuboid {
    /// quick check wheter two cuboids have any point in common
    fn overlap(&self, other: &Self) -> bool {
        overlap(&self.xs, &other.xs) && overlap(&self.ys, &other.ys) && overlap(&self.zs, &other.zs)
    }

    /// returns the intersection (if any) between self and other, of alignment opposite to other
    fn intersection(&self, other: &Self) -> Option<Cuboid> {
        if self.overlap(other) {
            Some(Cuboid {
                xs: intersection(&self.xs, &other.xs),
                ys: intersection(&self.ys, &other.ys),
                zs: intersection(&self.zs, &other.zs),
                on: !other.on,
            })
        } else {
            None
        }
    }

    fn volume(&self) -> usize {
        self.xs.len() * self.ys.len() * self.zs.len()
    }
}

/// quick check wether two range have any point in common
fn overlap(a: &Range<isize>, b: &Range<isize>) -> bool {
    a.contains(&b.start) || a.contains(&(b.end - 1))
}

/// returns the range of elements both in a and b
fn intersection(a: &Range<isize>, b: &Range<isize>) -> Range<isize> {
    a.start.max(b.start)..a.end.min(b.end)
}

fn parse(content: &str) -> Vec<Cuboid> {
    let mut instructions = vec![];
    for line in content.lines() {
        let split = line.split_whitespace().collect::<Vec<&str>>();
        let b = split[0] == "on";
        let coords = split[1].split(',').collect::<Vec<&str>>();
        let range = |s: &str| {
            let v = s.split('=').collect::<Vec<&str>>()[1]
                .split("..")
                .map(|s| s.parse().unwrap())
                .collect::<Vec<isize>>();
            v[0]..v[1] + 1
        };
        instructions.push(Cuboid {
            xs: range(coords[0]),
            ys: range(coords[1]),
            zs: range(coords[2]),
            on: b,
        });
    }
    instructions
}

fn filter_part1(instructions: Vec<Cuboid>) -> Vec<Cuboid> {
    let bound = Cuboid {
        xs: (-50..51),
        ys: (-50..51),
        zs: (-50..51),
        on: false,
    };
    instructions
        .iter()
        .filter(|c| c.overlap(&bound))
        .cloned()
        .collect()
}

fn solve(instructions: Vec<Cuboid>) -> usize {
    let mut cuboids: Vec<Cuboid> = vec![];
    let mut total = 0;
    for instruction in instructions {
        for i in 0..cuboids.len() {
            if let Some(intersection) = instruction.intersection(&cuboids[i]) {
                if intersection.on {
                    total += intersection.volume();
                } else {
                    total -= intersection.volume();
                }
                cuboids.push(intersection);
            }
        }
        if instruction.on {
            total += instruction.volume();
            cuboids.push(instruction);
        }
    }
    total
}

fn day22a(content: &str) -> usize {
    solve(filter_part1(parse(content)))
}

fn day22b(content: &str) -> usize {
    solve(parse(content))
}

fn main() {
    let content = include_str!("input22a.txt");
    println!("day22a: {}", day22a(content));
    println!("day22b: {}", day22b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let instructions = parse("on x=10..12,y=10..12,z=10..12");
        assert_eq!(1, instructions.len());
        let content = include_str!("test22a.txt");
        let instructions = parse(content);
        assert_eq!(4, instructions.len());
    }

    #[test]
    fn test_filter_part_1() {
        let content = include_str!("test22b.txt");
        let instructions = filter_part1(parse(content));
        assert_eq!(content.lines().count() - 2, instructions.len());
    }

    #[test]
    fn test_day22a() {
        let content = include_str!("test22a.txt");
        let take = |n| content.lines().take(n).collect::<Vec<&str>>().join("\n");
        assert_eq!(27, day22a(&take(1)));
        assert_eq!(46, day22a(&take(2)));
        assert_eq!(38, day22a(&take(3)));
        assert_eq!(39, day22a(content));
        let content = include_str!("test22b.txt");
        assert_eq!(590784, day22a(content));
    }

    #[test]
    fn test_day22b() {
        let content = include_str!("test22c.txt");
        assert_eq!(2758514936282235, day22b(content));
    }

    #[test]
    fn test_volume() {
        let a = Cuboid {
            xs: 0..2,
            ys: 3..6,
            zs: 0..5,
            on: true,
        };
        assert_eq!(2 * 3 * 5, a.volume());
    }

    #[test]
    fn test_diff() {
        let a = Cuboid {
            xs: 0..5,
            ys: 0..5,
            zs: 0..5,
            on: true,
        };
        let b = Cuboid {
            xs: -1..3,
            ys: 2..4,
            zs: 3..8,
            on: true,
        };
        assert_eq!(
            Some(Cuboid {
                xs: 0..3,
                ys: 2..4,
                zs: 3..5,
                on: false
            }),
            a.intersection(&b)
        );
    }
}
