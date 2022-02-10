use std::fs::read_to_string;

fn solve(numbers: &Vec<u32>, size: usize) -> u32 {
    let mut count = 0;
    let first: u32 = numbers[..size].iter().sum();
    println!("{} (N/A - no previous sum)", first);
    let windows = numbers.windows(size);
    for (current, next) in windows.zip(numbers[1..].windows(size)) {
        let current_sum: u32 = current.iter().sum();
        let next_sum: u32 = next.iter().sum();
        if next_sum > current_sum {
            println!("{} (increased)", next_sum);
            count += 1;
        } else {
            println!("{} (decreased)", next_sum);
        }
    }
    count
}

fn main() {
    // test data
    // let numbers = vec![199, 200, 208, 210, 200, 207, 240, 269, 260, 263];

    let content = read_to_string("input.txt").unwrap_or_default();
    let mut numbers: Vec<u32> = Vec::default();
    for line in content.lines() {
        let number: u32 = line.parse().unwrap_or_default();
        numbers.push(number);
    }

    let sol01a = solve(&numbers, 1);
    let sol01b = solve(&numbers, 3);

    println!("sol01a: {}", sol01a);
    println!("sol01b: {}", sol01b);
}
