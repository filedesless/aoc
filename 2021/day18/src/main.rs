/// (number, depth)
type Numbers = Vec<(usize, usize)>;

fn parse(content: &str) -> Numbers {
    let mut v = vec![];
    let mut depth = 0;
    let mut iter = content.chars().peekable();
    while let Some(c) = iter.next() {
        match c {
            '[' => depth += 1,
            ']' => depth -= 1,
            c if c.is_digit(10) => {
                let mut s = String::from_iter(vec![c]);
                while let Some(d) = iter.next_if(|next_c| next_c.is_digit(10)) {
                    s.push(d);
                }
                v.push((s.parse().unwrap(), depth));
            }
            _ => {}
        }
    }
    v
}

fn explode(mut numbers: Numbers, i: usize) -> Numbers {
    assert!(i + 1 < numbers.len());
    assert_eq!(5, numbers[i].1);
    assert_eq!(5, numbers[i + 1].1);

    if i > 0 {
        numbers[i - 1].0 += numbers[i].0;
    }
    if i + 2 < numbers.len() {
        numbers[i + 2].0 += numbers[i + 1].0;
    }
    numbers.remove(i);
    numbers[i] = (0, 4);
    numbers
}

fn split(mut numbers: Numbers, i: usize) -> Numbers {
    assert!(i < numbers.len());
    let n = numbers[i].0;
    assert!(n >= 10);

    let l = n / 2;
    let r = n - l;
    let d = numbers[i].1 + 1;
    numbers[i] = (r, d);
    numbers.insert(i, (l, d));
    numbers
}

fn reduce(mut numbers: Numbers) -> Numbers {
    if let Some(i) = numbers.iter().position(|&(_, d)| d == 5) {
        numbers = reduce(explode(numbers, i));
    }
    if let Some(i) = numbers.iter().position(|&(n, _)| n >= 10) {
        numbers = reduce(split(numbers, i));
    }
    numbers
}

fn add(left: Numbers, right: Numbers) -> Numbers {
    let f = |&(n, d)| (n, d + 1);
    reduce(left.iter().chain(right.iter()).map(f).collect())
}

fn sum<I>(l: I) -> Numbers
where
    I: Iterator<Item = Numbers>,
{
    l.reduce(add).unwrap()
}

fn magnitude(mut numbers: Numbers) -> usize {
    for depth in (1..=4).rev() {
        while let Some(i) = numbers.iter().position(|(_, d)| d == &depth) {
            let (l, _) = numbers.remove(i);
            let (r, _) = numbers.remove(i);
            numbers.insert(i, ((3 * l + 2 * r), depth - 1));
        }
    }
    numbers[0].0
}

fn day18a(content: &str) -> usize {
    magnitude(sum(content.lines().map(parse)))
}

fn day18b(content: &str) -> usize {
    let list: Vec<Numbers> = content.lines().map(parse).collect();

    let f = |i: usize, j: usize| magnitude(add(list[i].clone(), list[j].clone()));
    (0..list.len())
        .flat_map(|i| (0..list.len()).map(move |j| f(i, j)))
        .max()
        .unwrap()
}

fn main() {
    let content = include_str!("input18a.txt");
    println!("day18a: {}", day18a(content));
    println!("day18b: {}", day18b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(vec![(1, 1), (2, 1)], parse("[1, 2]"));
        assert_eq!(vec![(1, 2), (2, 2), (3, 1)], parse("[[1, 2], 3]"));
        assert_eq!(vec![(9, 1), (8, 2), (7, 2)], parse("[9, [8, 7]]"));
        assert_eq!(
            vec![(9, 5), (8, 5), (1, 4), (2, 3), (3, 2), (4, 1)],
            parse("[[[[[9,8],1],2],3],4]")
        );
        assert_eq!(
            vec![(0, 4), (9, 4), (2, 3), (3, 2), (4, 1)],
            parse("[[[[0,9],2],3],4")
        );
        assert_eq!(
            vec![
                (0, 4),
                (7, 4),
                (4, 3),
                (7, 4),
                (8, 4),
                (0, 4),
                (13, 4),
                (1, 2),
                (1, 2)
            ],
            parse("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
        );
    }

    #[test]
    fn test_explode() {
        assert_eq!(
            parse("[[[[0,9],2],3],4"),
            explode(parse("[[[[[9,8],1],2],3],4]"), 0)
        );
        assert_eq!(
            parse("[7,[6,[5,[7,0]]]]"),
            explode(parse("[7,[6,[5,[4,[3,2]]]]]"), 4)
        );
        assert_eq!(
            parse("[[6,[5,[7,0]]],3]"),
            explode(parse("[[6,[5,[4,[3,2]]]],1]"), 3)
        );
        assert_eq!(
            parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
            explode(parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"), 3)
        );
        assert_eq!(
            parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
            explode(parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"), 7)
        );
    }

    #[test]
    fn test_split() {
        assert_eq!(
            parse("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"),
            split(parse("[[[[0,7],4],[15,[0,13]]],[1,1]]"), 3)
        );
        assert_eq!(
            parse("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]"),
            split(parse("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"), 6)
        );
    }

    #[test]
    fn test_add() {
        assert_eq!(
            parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"),
            add(parse("[[[[4,3],4],4],[7,[[8,4],9]]]"), parse("[1,1]"))
        )
    }

    #[test]
    fn test_sum() {
        let content = include_str!("sum1.txt");
        let l = content.lines().map(parse);
        assert_eq!(parse("[[[[1,1],[2,2]],[3,3]],[4,4]]"), sum(l));
        let content = include_str!("sum2.txt");
        let l = content.lines().map(parse);
        assert_eq!(parse("[[[[3,0],[5,3]],[4,4]],[5,5]]"), sum(l));
        let content = include_str!("sum3.txt");
        let l = content.lines().map(parse);
        assert_eq!(
            parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
            sum(l)
        );
    }

    #[test]
    fn test_magnitude() {
        assert_eq!(29, magnitude(parse("[9,1]")));
        assert_eq!(21, magnitude(parse("[1,9]")));
        assert_eq!(129, magnitude(parse("[[9,1],[1,9]]")));
        assert_eq!(143, magnitude(parse("[[1,2],[[3,4],5]]")));
        assert_eq!(1384, magnitude(parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")));
    }

    #[test]
    fn test_day18a() {
        let content = include_str!("sum4.txt");
        let expected = parse("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]");
        assert_eq!(expected, sum(content.lines().map(parse)));
        assert_eq!(4140, magnitude(expected));
    }
}
