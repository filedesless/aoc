use std::collections::HashMap;
use std::collections::HashSet;

fn diff(a: String, b: String) -> String {
    let s1: HashSet<char> = HashSet::from_iter(a.chars());
    let s2 = HashSet::from_iter(b.chars());
    let res = String::from_iter(s1.difference(&s2));
    res
}

fn deduce(signals: &str, words: &str) -> String {
    let mut decoder: HashMap<String, char> = HashMap::new();
    let mut one: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| signal.len() == 2)
        .unwrap()
        .chars()
        .collect();
    one.sort();
    decoder.insert(String::from_iter(one.iter()), '1');
    let mut four: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| signal.len() == 4)
        .unwrap()
        .chars()
        .collect();
    four.sort();
    decoder.insert(String::from_iter(four.iter()), '4');
    let mut seven: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| signal.len() == 3)
        .unwrap()
        .chars()
        .collect();
    seven.sort();
    decoder.insert(String::from_iter(seven.iter()), '7');
    let mut eight: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| signal.len() == 7)
        .unwrap()
        .chars()
        .collect();
    eight.sort();
    decoder.insert(String::from_iter(eight.iter()), '8');

    let mut six: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 6 && // 6, 9 or 0
            diff(String::from(signal), one.iter().collect()).len() == 5
        })
        .unwrap()
        .chars()
        .collect();
    six.sort();
    decoder.insert(String::from_iter(six.iter()), '6');

    let mut zero: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 6 // 6, 9 or 0
                && !diff(String::from(signal), six.iter().collect()).is_empty() // not 6
                && !diff(four.iter().collect(), signal.to_string()).is_empty()
        })
        .unwrap()
        .chars()
        .collect();
    zero.sort();
    decoder.insert(String::from_iter(zero.iter()), '0');

    let mut nine: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 6 // 6, 9 or 0
                && !diff(String::from(signal), six.iter().collect()).is_empty() // not 6
                && diff(four.iter().collect(), signal.to_string()).is_empty()
        })
        .unwrap()
        .chars()
        .collect();
    nine.sort();
    decoder.insert(String::from_iter(nine.iter()), '9');

    let mut three: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 5 // 2, 3 or 5
                && diff(one.iter().collect(), String::from(signal)).is_empty()
        })
        .unwrap()
        .chars()
        .collect();
    three.sort();
    decoder.insert(String::from_iter(three.iter()), '3');

    let mut five: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 5 // 2, 3 or 5
                && !diff(one.iter().collect(), String::from(signal)).is_empty() // not 3
                && diff(six.iter().collect(), String::from(signal)).len() == 1
        })
        .unwrap()
        .chars()
        .collect();
    five.sort();
    decoder.insert(String::from_iter(five.iter()), '5');

    let mut two: Vec<char> = signals
        .split_whitespace()
        .find(|&signal| {
            signal.len() == 5 // 2, 3 or 5
                && !diff(one.iter().collect(), String::from(signal)).is_empty() // not 3
                && diff(six.iter().collect(), String::from(signal)).len() == 2
        })
        .unwrap()
        .chars()
        .collect();
    two.sort();
    decoder.insert(String::from_iter(two.iter()), '2');

    let mut result = String::default();
    for word in words.split_whitespace() {
        let mut v: Vec<char> = word.chars().collect();
        v.sort();
        let c = decoder.get(&String::from_iter(v.iter())).unwrap();
        result.push(*c);
    }

    result
}

fn day08a(content: &str) -> usize {
    let valid_counts = [2, 4, 3, 7];
    content
        .lines()
        .map(|line| {
            let output = line.split(" | ").collect::<Vec<&str>>()[1];
            output
                .split_whitespace()
                .filter(|digit| {
                    valid_counts.contains(&digit.chars().collect::<HashSet<char>>().len())
                })
                .count()
        })
        .sum()
}

fn day08b(content: &str) -> usize {
    let mut sum = 0;
    for line in content.lines() {
        let mut split = line.split(" | ");
        let signals = split.nth(0).unwrap();
        let words = split.nth(0).unwrap();
        let n: usize = deduce(signals, words).parse().unwrap();
        sum += n;
    }
    sum
}

fn main() {
    let content = include_str!("input08a.txt");
    println!("day08a: {}", day08a(content));
    println!("day08b: {}", day08b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day08a() {
        let content = include_str!("test08a.txt");
        assert_eq!(26, day08a(content));
    }

    #[test]
    fn test_deduce() {
        let signals = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab";
        assert_eq!(String::from("8523796401"), deduce(signals, signals));
        assert_eq!(
            String::from("5353"),
            deduce(signals, "cdfeb fcadb cdfeb cdbaf")
        );
        let answers = [8394, 9781, 1197, 9361, 4873, 8418, 4548, 1625, 8717, 4315];
        let content = include_str!("test08a.txt");
        for (i, line) in content.lines().enumerate() {
            let mut split = line.split(" | ");
            let signals = split.nth(0).unwrap();
            let words = split.nth(0).unwrap();
            assert_eq!(answers[i].to_string(), deduce(signals, words));
        }
    }

    #[test]
    fn test_day08b() {
        let content = include_str!("test08a.txt");
        assert_eq!(61229, day08b(content));
    }
}
