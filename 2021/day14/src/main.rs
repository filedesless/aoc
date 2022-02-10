use std::collections::HashMap;

type Pair = (char, char);
type CharCount = HashMap<char, usize>;
type Pairs = HashMap<Pair, usize>;
type Rules = HashMap<Pair, char>;

fn count_pairs(line: &str) -> Pairs {
    let mut pairs: Pairs = HashMap::new();
    for window in line.chars().collect::<Vec<char>>()[..].windows(2) {
        *pairs.entry((window[0], window[1])).or_default() += 1;
    }
    pairs
}

fn count_chars(pairs: &Pairs, last: char) -> CharCount {
    let mut count = HashMap::new();

    for ((a, _), n) in pairs {
        *count.entry(*a).or_default() += n;
    }

    *count.entry(last).or_default() += 1;

    count
}

fn parse(content: &str) -> (Pairs, Rules) {
    let mut pairs = HashMap::new();
    let mut rules = HashMap::new();
    let mut read_pairs = true;
    for line in content.lines() {
        if line.is_empty() {
            read_pairs = false;
        } else if read_pairs {
            pairs.extend(count_pairs(line));
        } else {
            let v: Vec<Vec<char>> = line.split(" -> ").map(|s| s.chars().collect()).collect();
            rules.insert((v[0][0], v[0][1]), v[1][0]);
        }
    }
    (pairs, rules)
}

fn step(pairs: Pairs, rules: &Rules) -> Pairs {
    let mut count = HashMap::new();
    for ((a, b), n) in pairs {
        if let Some(c) = rules.get(&(a, b)) {
            *count.entry((a, *c)).or_default() += n;
            *count.entry((*c, b)).or_default() += n;
        } else {
            *count.entry((a, b)).or_default() += n;
        }
    }
    count
}

fn simulate(mut pairs: Pairs, rules: &Rules, n: usize) -> Pairs {
    for _ in 1..=n {
        pairs = step(pairs, rules);
    }
    pairs
}

fn solve(pairs: Pairs, last: char) -> usize {
    let result = count_chars(&pairs, last);
    let (_, max) = result.iter().max_by_key(|(_, &v)| v).unwrap();
    let (_, min) = result.iter().min_by_key(|(_, &v)| v).unwrap();
    max - min
}

fn day14a(content: &str) -> usize {
    let (pairs, rules) = parse(content);
    let last = content.lines().next().unwrap().chars().last().unwrap();
    solve(simulate(pairs, &rules, 10), last)
}

fn day14b(content: &str) -> usize {
    let (pairs, rules) = parse(content);
    let last = content.lines().next().unwrap().chars().last().unwrap();
    solve(simulate(pairs, &rules, 40), last)
}

fn main() {
    let content = include_str!("input14a.txt");
    println!("day14a: {}", day14a(content));
    println!("day14b: {}", day14b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_pairs() {
        assert_eq!(1, count_pairs("AA").len());
        assert_eq!(1, count_pairs("AAA").len());
        assert_eq!(2, count_pairs("AAB").len());
        assert_eq!(3, count_pairs("ABCD").len());
    }

    #[test]
    fn test_parse() {
        let content = include_str!("test14a.txt");
        let (pairs, rules) = parse(content);
        assert_eq!(count_pairs("NNCB"), pairs);
        assert_eq!(16, rules.len());
    }

    #[test]
    fn test_step() {
        let content = include_str!("test14a.txt");
        let (pairs, rules) = parse(content);
        let pairs = step(pairs, &rules);
        assert_eq!(count_pairs("NCNBCHB"), pairs);
    }

    #[test]
    fn test_simulate() {
        let content = include_str!("test14a.txt");
        let (pairs, rules) = parse(content);
        assert_eq!(count_pairs("NCNBCHB"), simulate(pairs.clone(), &rules, 1));
        assert_eq!(
            count_pairs("NBCCNBBBCBHCB"),
            simulate(pairs.clone(), &rules, 2)
        );
        assert_eq!(
            count_pairs("NBBBCNCCNBBNBNBBCHBHHBCHB"),
            simulate(pairs.clone(), &rules, 3)
        );
        assert_eq!(
            count_pairs("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"),
            simulate(pairs, &rules, 4)
        );
    }

    #[test]
    fn test_count_chars() {
        let content = include_str!("test14a.txt");
        let (pairs, rules) = parse(content);
        assert_eq!(
            97,
            count_chars(&simulate(pairs.clone(), &rules, 5), 'B')
                .values()
                .sum::<usize>()
        );

        let char_count = count_chars(&simulate(pairs, &rules, 10), 'B');
        assert_eq!(3073, char_count.values().sum::<usize>());
        assert_eq!(&1749, char_count.get(&'B').unwrap());
        assert_eq!(&298, char_count.get(&'C').unwrap());
        assert_eq!(&161, char_count.get(&'H').unwrap());
        assert_eq!(&865, char_count.get(&'N').unwrap());
    }

    #[test]
    fn test_day14a() {
        let content = include_str!("test14a.txt");
        assert_eq!(1588, day14a(content));
    }

    #[test]
    fn test_day14b() {
        let content = include_str!("test14a.txt");
        assert_eq!(2188189693529, day14b(content));
    }
}
