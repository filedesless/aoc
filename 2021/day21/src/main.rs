use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Player {
    pos: usize,
    score: usize,
}

fn roll(player: Player, value: usize) -> Player {
    let pos = (player.pos + value - 1) % 10 + 1;
    Player {
        pos,
        score: player.score + pos,
    }
}

fn simulate_a<I>(player: Player, other: Player, mut die: I, rolls: usize) -> usize
where
    I: Iterator<Item = usize>,
{
    if other.score >= 1000 {
        player.score * rolls
    } else {
        simulate_a(
            other,
            roll(player, die.by_ref().take(3).sum()),
            die,
            rolls + 3,
        )
    }
}

/// sum of 3 3-sided dice only has 7 distinct outcomes
/// no need to simulate all 27 possibilities, only each result and how many times it happens
const REPEAT: [usize; 10] = [0, 0, 0, 1, 3, 6, 7, 6, 3, 1];

/// (a wins, b wins)
fn simulate_b(
    cache: &mut HashMap<(Player, Player), (usize, usize)>,
    player: Player,
    other: Player,
) -> (usize, usize) {
    if other.score >= 21 {
        (0, 1)
    } else if let Some(result) = cache.get(&(player, other)) {
        *result
    } else {
        let result = (3..=9)
            .map(|i| {
                let (a, b) = simulate_b(cache, other, roll(player, i));
                (REPEAT[i] * a, REPEAT[i] * b)
            })
            .fold((0, 0), |(a, b), (x, y)| (a + y, b + x));
        cache.insert((player, other), result);
        result
    }
}

fn day21a(a: Player, b: Player) -> usize {
    simulate_a(a, b, (1..=100).cycle(), 0)
}

fn day21b(a: Player, b: Player) -> usize {
    let mut cache = HashMap::new();
    let (x, y) = simulate_b(&mut cache, a, b);
    x.max(y)
}

fn main() {
    let (a, b) = (Player { pos: 6, score: 0 }, Player { pos: 8, score: 0 });
    println!("day21a: {}", day21a(a, b));
    println!("day21b: {}", day21b(a, b));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day21a() {
        assert_eq!(
            739785,
            day21a(Player { pos: 4, score: 0 }, Player { pos: 8, score: 0 })
        );
    }

    #[test]
    fn test_simulate_b() {
        let mut cache = HashMap::new();
        assert_eq!(
            (444356092776315, 341960390180808),
            simulate_b(
                &mut cache,
                Player { pos: 4, score: 0 },
                Player { pos: 8, score: 0 }
            )
        );
    }
}
