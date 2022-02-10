#[derive(Debug)]
struct Point {
    x: i32,
    y: i32,
}

struct Probe {
    vel: Point,
    pos: Point,
}

impl Probe {
    fn new(dx: i32, dy: i32) -> Self {
        Self {
            pos: Point { x: 0, y: 0 },
            vel: Point { x: dx, y: dy },
        }
    }

    fn step(&mut self) {
        self.pos.x += self.vel.x;
        self.pos.y += self.vel.y;
        self.vel.y -= 1;
        if self.vel.x > 0 {
            self.vel.x -= 1;
        }
    }

    fn simulate(&mut self, (min, max): &(Point, Point)) -> bool {
        while self.pos.y >= min.y {
            self.step();
            if (min.x..=max.x).contains(&self.pos.x) && (min.y..=max.y).contains(&self.pos.y) {
                return true;
            }
        }
        false
    }
}

fn parse(content: &str) -> (i32, i32, i32, i32) {
    let v: Vec<i32> = content
        .split(|c: char| !c.is_digit(10) && c != '-')
        .filter(|s| !s.is_empty())
        .map(|s| s.parse::<i32>().unwrap())
        .collect();

    (v[0], v[1], v[2], v[3])
}

fn day17a(content: &str) -> i32 {
    /*
    - y starts at 0, goes up by a decrementing amount starting at dy until peak at \sum{i=1}{dy}{i} == (dy * (dy + 1) / 2)
    - y goes from peak to 0 by steps of increasing length up to the initial dy
    - next step y decreases by (dy + 1), the maximal value dy can have is therefore -(ymin + 1)
        - any dy above this threshold will overshoot at this step
        - any dy below will have a lower peak
    */
    let (_, _, ymin, _) = parse(content);
    let dy = -(ymin + 1);
    dy * (dy + 1) / 2
}

fn day17b(content: &str) -> usize {
    /*
    - dx is at least 0
    - dx is at most xmax
    - dy is at least ymin
    - dy is at most -(ymin + 1)

    - a velocity is valid when simulation is within bounds and invalid when y goes below ymin
    */
    let (xmin, xmax, ymin, ymax) = parse(content);
    let target = (Point { x: xmin, y: ymin }, Point { x: xmax, y: ymax });

    let mut count = 0;
    for dx in 0..=xmax {
        for dy in ymin..=-(ymin + 1) {
            if Probe::new(dx, dy).simulate(&target) {
                count += 1;
            }
        }
    }

    count
}

fn main() {
    let content = include_str!("input17a.txt");
    println!("day17a: {}", day17a(content));
    println!("day17b: {}", day17b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test17a.txt");
        let rect = parse(content);
        assert_eq!((20, 30, -10, -5), rect);
    }

    #[test]
    fn test_day17a() {
        let content = include_str!("test17a.txt");
        assert_eq!(45, day17a(content));
    }

    #[test]
    fn test_simulate() {
        let content = include_str!("test17a.txt");
        let (xmin, xmax, ymin, ymax) = parse(content);
        let bounds = (Point { x: xmin, y: ymin }, Point { x: xmax, y: ymax });
        assert!(Probe::new(7, 2).simulate(&bounds));
        assert!(Probe::new(7, 3).simulate(&bounds));
        assert!(Probe::new(9, 0).simulate(&bounds));
        assert!(!Probe::new(17, -4).simulate(&bounds));
    }

    #[test]
    fn test_day17b() {
        let content = include_str!("test17a.txt");
        assert_eq!(112, day17b(content));
    }
}
