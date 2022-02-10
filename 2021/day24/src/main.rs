use itertools::Itertools;

/// implemented the ALU as reference
fn run(program: &str, input: &[isize]) -> Vec<isize> {
    let mut state: Vec<isize> = vec![0; 4];
    let mut i = 0;
    let reg = |c: &str| (c.chars().next().unwrap() as u8 - b'w') as usize;
    let src = |s: &str| s.parse::<isize>();
    for line in program.lines() {
        let split = line.split_whitespace().collect::<Vec<&str>>();
        match split[0] {
            "inp" => {
                state[reg(split[1])] = input[i];
                i += 1;
            }
            "add" => {
                state[reg(split[1])] += src(split[2]).unwrap_or_else(|_| state[reg(split[2])]);
            }
            "mul" => {
                state[reg(split[1])] *= src(split[2]).unwrap_or_else(|_| state[reg(split[2])]);
            }
            "div" => {
                state[reg(split[1])] /= src(split[2]).unwrap_or_else(|_| state[reg(split[2])]);
            }
            "mod" => {
                state[reg(split[1])] %= src(split[2]).unwrap_or_else(|_| state[reg(split[2])]);
            }
            "eql" => {
                state[reg(split[1])] = (state[reg(split[1])]
                    == src(split[2]).unwrap_or_else(|_| state[reg(split[2])]))
                    as isize;
            }
            _ => {}
        }
    }
    state
}

const D: [isize; 14] = [1, 1, 1, 1, 1, 26, 1, 26, 26, 1, 26, 26, 26, 26];
const K1: [isize; 14] = [12, 11, 13, 11, 14, -10, 11, -9, -3, 13, -5, -10, -4, -5];
const K2: [isize; 14] = [4, 11, 5, 11, 14, 7, 11, 4, 6, 5, 9, 12, 14, 14];

fn digit(z: isize, w: isize, i: usize) -> isize {
    if z % 26 + K1[i] != w {
        z / D[i] * 26 + w + K2[i]
    } else {
        z / D[i]
    }
}

/// re-implementing the monad program in rust
fn monad(input: &[isize]) -> isize {
    let mut z = 0;
    for (i, &w) in input.iter().enumerate() {
        z = digit(z, w, i);
    }
    z
}

/// cutting down the search space to find
fn search(i: usize, z: isize, path: &mut Vec<isize>, asc: bool) -> bool {
    if i == 14 {
        return z == 0;
    }

    let mut candidates: Vec<isize> = (1..10).collect();
    if !asc {
        candidates.reverse();
    }

    for w in candidates {
        if D[i] == 26 && (z % 26) + K1[i] != w {
            continue;
        }
        path.push(w);
        if search(i + 1, digit(z, w, i), path, asc) {
            return true;
        }
        path.pop();
    }

    false
}

fn day24a() -> String {
    let mut path = vec![];
    search(0, 0, &mut path, false);
    path.iter().map(|&d| d as usize).join("")
}

fn day24b() -> String {
    let mut path = vec![];
    search(0, 0, &mut path, true);
    path.iter().map(|&d| d as usize).join("")
}

fn main() {
    println!("day24a: {}", day24a());
    println!("day24b: {}", day24b());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_neg() {
        let program = include_str!("neg.txt");
        for i in 0..100 {
            assert_eq!(-i, run(program, &[i])[1]);
        }
    }

    #[test]
    fn test_run_mul() {
        let program = include_str!("mul.txt");
        for z in 0..100 {
            for x in 0..100 {
                assert_eq!(if z * 3 == x { 1 } else { 0 }, run(program, &[z, x])[3]);
            }
        }
    }

    #[test]
    fn test_run_bits() {
        let program = include_str!("bits.txt");
        for w in 0..100 {
            println!("w: {}", w);
            assert_eq!(
                [(w & 8) >> 3, (w & 4) >> 2, (w & 2) >> 1, w & 1],
                run(program, &[w])[..4]
            );
        }
    }

    #[test]
    fn test_monad() {
        let program = include_str!("monad.txt");
        let input = [1, 3, 5, 7, 9, 2, 4, 6, 8, 9, 9, 9, 9, 9];
        assert_eq!(run(program, &input)[3], monad(&input));
    }

    #[test]
    fn test_day24a() {
        let mut path = vec![];
        search(0, 0, &mut path, false);
        assert!(monad(&path) == 0);
        let program = include_str!("monad.txt");
        assert!(run(program, &path)[3] == 0);
    }
}
