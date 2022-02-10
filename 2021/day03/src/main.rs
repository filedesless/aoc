/// returns (ones, zeroes) for a given col
fn count<'a, T>(lines: T, col: usize) -> (usize, usize)
where
    T: Iterator<Item = &'a str>,
{
    let mut ones = 0;
    let mut zeroes = 0;
    for line in lines {
        let bit = &line[col..=col];
        if bit == "1" {
            ones += 1;
        } else {
            zeroes += 1;
        }
    }
    (ones, zeroes)
}

fn day03a(content: &str) -> usize {
    let len = content.lines().next().unwrap().len();
    let mut gamma = String::new();
    let mut epsilon = String::new();
    for col in 0..len {
        let (ones, zeroes) = count(content.lines(), col);
        if ones > zeroes {
            gamma.push('1');
            epsilon.push('0');
        } else {
            gamma.push('0');
            epsilon.push('1');
        }
    }
    usize::from_str_radix(&gamma, 2).unwrap() * usize::from_str_radix(&epsilon, 2).unwrap()
}

/// returns (o2, co2)
fn day03b(content: &str) -> (usize, usize) {
    let len = content.lines().next().unwrap().len();
    let mut o2_prefix = String::new();
    let mut o2 = 0;
    let mut co2_prefix = String::new();
    let mut co2 = 0;
    for col in 0..=len {
        let mut o2_lines = content.lines().filter(|line| line.starts_with(&o2_prefix));
        let n = o2_lines.clone().count();
        if n == 1 {
            let value = o2_lines.next().unwrap();
            o2 = usize::from_str_radix(&value, 2).unwrap();
        } else {
            let (ones, zeroes) = count(o2_lines, col);
            if zeroes > ones {
                o2_prefix.push('0');
            } else {
                o2_prefix.push('1');
            }
        }
        let mut co2_lines = content.lines().filter(|line| line.starts_with(&co2_prefix));
        if co2_lines.clone().count() == 1 {
            let value = co2_lines.next().unwrap();
            co2 = usize::from_str_radix(&value, 2).unwrap();
        } else {
            let (ones, zeroes) = count(co2_lines, col);
            if zeroes > ones {
                co2_prefix.push('1');
            } else {
                co2_prefix.push('0');
            }
        }
    }
    (o2, co2)
}

fn main() {
    let content = include_str!("input03a.txt");
    println!("Day03a: {}", day03a(content));
    let (o2, co2) = day03b(content);
    println!("Day03b: {} (o2: {} co2: {})", o2 * co2, o2, co2);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_day03a() {
        let content = include_str!("test03a.txt");
        let power = day03a(content);
        assert_eq!(22 * 9, power);
    }

    #[test]
    fn test_day03b() {
        let content = include_str!("test03a.txt");
        let (o2, co2) = day03b(content);
        assert_eq!(23, o2);
        assert_eq!(10, co2);
    }
}
