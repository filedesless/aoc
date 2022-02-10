enum ParseResult {
    Complete,
    Incomplete(Vec<char>),
    Corrupted(char),
}

fn parse(line: &str) -> ParseResult {
    let mut stack = vec![];
    for found in line.chars() {
        match found {
            '[' | '(' | '<' | '{' => stack.push(found),
            _ => {
                if let Some(last) = stack.pop() {
                    let expected = matching(&last);
                    if found != expected {
                        return ParseResult::Corrupted(found);
                    }
                }
            }
        }
    }
    if stack.is_empty() {
        ParseResult::Complete
    } else {
        ParseResult::Incomplete(stack)
    }
}

fn matching(c: &char) -> char {
    match c {
        '[' => ']',
        '(' => ')',
        '{' => '}',
        '<' => '>',
        _ => panic!(),
    }
}

fn find_corrupted(line: &str) -> Option<char> {
    match parse(line) {
        ParseResult::Corrupted(c) => Some(c),
        _ => None,
    }
}

fn find_completion(line: &str) -> Option<String> {
    match parse(line) {
        ParseResult::Incomplete(v) => Some(String::from_iter(v.iter().map(matching).rev())),
        _ => None,
    }
}

fn day10a(content: &str) -> usize {
    content
        .lines()
        .filter_map(find_corrupted)
        .map(|c| match c {
            ')' => 3,
            ']' => 57,
            '}' => 1197,
            '>' => 25137,
            _ => 0,
        })
        .sum()
}

fn completion_score(completion: &str) -> usize {
    let mut score = 0;
    for c in completion.chars() {
        score *= 5;
        score += match c {
            ')' => 1,
            ']' => 2,
            '}' => 3,
            '>' => 4,
            _ => 0,
        }
    }
    score
}

fn day10b(content: &str) -> usize {
    let mut scores: Vec<usize> = content
        .lines()
        .filter_map(find_completion)
        .map(|completion| completion_score(&completion))
        .collect();
    scores.sort();
    scores[scores.len() / 2]
}

fn main() {
    let content = include_str!("input10a.txt");
    println!("day10a: {}", day10a(content));
    println!("day10b: {}", day10b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_error() {
        let content = include_str!("test10a.txt");
        let answers = ['}', ')', ']', ')', '>'];
        for (i, found) in content.lines().filter_map(find_corrupted).enumerate() {
            assert_eq!(answers[i], found);
        }
    }

    #[test]
    fn test_day10a() {
        let content = include_str!("test10a.txt");
        assert_eq!(26397, day10a(content));
    }

    #[test]
    fn test_find_completion() {
        let content = include_str!("test10a.txt");
        let answers = ["}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>"];
        for (i, line) in content.lines().filter_map(find_completion).enumerate() {
            assert_eq!(String::from(answers[i]), line);
        }
    }

    #[test]
    fn test_completion_score() {
        let completions = ["}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>"];
        let answers = [288957, 5566, 1480781, 995444, 294];
        for (i, completion) in completions.iter().enumerate() {
            assert_eq!(answers[i], completion_score(completion));
        }
    }

    #[test]
    fn test_day10b() {
        let content = include_str!("test10a.txt");
        assert_eq!(288957, day10b(content));
    }
}
