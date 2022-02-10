use std::collections::VecDeque;

fn day06a(initial: &Vec<usize>, until: usize) -> usize {
    let mut state = initial.clone();
    let mut day = 0;
    // println!("Initial state: {:?}", state);
    while day < until {
        let mut new = 0;
        for fish in &mut *state {
            if *fish == 0 {
                *fish = 7;
                new += 1;
            }
            *fish -= 1;
        }
        for _ in 0..new {
            state.push(8);
        }
        day += 1;
        // println!("After day {}", day);
    }
    state.len()
}

fn day06b(initial: &Vec<usize>, until: usize) -> usize {
    let mut state: VecDeque<usize> = VecDeque::from(vec![0, 0, 0, 0, 0, 0, 0, 0, 0]);
    for &fish in initial {
        state[fish] += 1;
    }
    // println!("Initial state: {:?}", state);
    let mut day = 0;
    while day < until {
        let count = state.pop_front().unwrap();
        state[6] += count;
        state.push_back(count);
        day += 1;
        // println!("After day {}: {:?}", day, state);
    }
    state.iter().sum()
}

fn main() {
    let initial = vec![
        4, 1, 4, 1, 3, 3, 1, 4, 3, 3, 2, 1, 1, 3, 5, 1, 3, 5, 2, 5, 1, 5, 5, 1, 3, 2, 5, 3, 1, 3,
        4, 2, 3, 2, 3, 3, 2, 1, 5, 4, 1, 1, 1, 2, 1, 4, 4, 4, 2, 1, 2, 1, 5, 1, 5, 1, 2, 1, 4, 4,
        5, 3, 3, 4, 1, 4, 4, 2, 1, 4, 4, 3, 5, 2, 5, 4, 1, 5, 1, 1, 1, 4, 5, 3, 4, 3, 4, 2, 2, 2,
        2, 4, 5, 3, 5, 2, 4, 2, 3, 4, 1, 4, 4, 1, 4, 5, 3, 4, 2, 2, 2, 4, 3, 3, 3, 3, 4, 2, 1, 2,
        5, 5, 3, 2, 3, 5, 5, 5, 4, 4, 5, 5, 4, 3, 4, 1, 5, 1, 3, 4, 4, 1, 3, 1, 3, 1, 1, 2, 4, 5,
        3, 1, 2, 4, 3, 3, 5, 4, 4, 5, 4, 1, 3, 1, 1, 4, 4, 4, 4, 3, 4, 3, 1, 4, 5, 1, 2, 4, 3, 5,
        1, 1, 2, 1, 1, 5, 4, 2, 1, 5, 4, 5, 2, 4, 4, 1, 5, 2, 2, 5, 3, 3, 2, 3, 1, 5, 5, 5, 4, 3,
        1, 1, 5, 1, 4, 5, 2, 1, 3, 1, 2, 4, 4, 1, 1, 2, 5, 3, 1, 5, 2, 4, 5, 1, 2, 3, 1, 2, 2, 1,
        2, 2, 1, 4, 1, 3, 4, 2, 1, 1, 5, 4, 1, 5, 4, 4, 3, 1, 3, 3, 1, 1, 3, 3, 4, 2, 3, 4, 2, 3,
        1, 4, 1, 5, 3, 1, 1, 5, 3, 2, 3, 5, 1, 3, 1, 1, 3, 5, 1, 5, 1, 1, 3, 1, 1, 1, 1, 3, 3, 1,
    ];
    println!("day06a: {}", day06a(&initial, 80));
    println!("day06b: {}", day06b(&initial, 256));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day06a() {
        let initial: Vec<usize> = vec![3, 4, 3, 1, 2];
        assert_eq!(26, day06a(&initial, 18));
        assert_eq!(5934, day06a(&initial, 80));
    }

    #[test]
    fn test_day06b() {
        let initial: Vec<usize> = vec![3, 4, 3, 1, 2];
        assert_eq!(26, day06b(&initial, 18));
        assert_eq!(5934, day06b(&initial, 80));
        assert_eq!(26984457539, day06b(&initial, 256));
    }
}
