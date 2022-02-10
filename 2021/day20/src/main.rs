type Alg = Vec<bool>;
type Img = Vec<Vec<bool>>;

fn parse(content: &str) -> (Alg, Img) {
    let mut lines = content.lines();
    let p = |c: char| c == '#';
    let alg = lines
        .next()
        .map(|line| line.chars().map(p).collect())
        .unwrap_or_default();
    lines.next();
    let img = lines.map(|line| line.chars().map(p).collect()).collect();
    (alg, img)
}

/// uses 9 pixels around given coord to compute an index into the alg
fn alg_index(img: &Img, bg: bool, lin: usize, col: usize) -> usize {
    let mut sum = 0;
    let mut bit = 0b1_0000_0000;

    for mi in [lin.checked_sub(1), Some(lin), lin.checked_add(1)] {
        for mj in [col.checked_sub(1), Some(col), col.checked_add(1)] {
            if let Some(true) = mi
                .and_then(|i| img.get(i))
                .and_then(|l| mj.and_then(|j| l.get(j)))
                .or(Some(&bg))
            {
                sum |= bit
            }
            bit >>= 1
        }
    }
    sum
}

/// count the lit pixel in a given image
fn lit_pixels(img: &Img) -> usize {
    img.iter()
        .map(|line| line.iter().filter(|&&b| b).count())
        .sum()
}

/// applies the alg onto a given image
fn apply_alg(mut img: Img, bg: &mut bool, alg: &[bool]) -> Img {
    let bg_line = vec![*bg; img[0].len()];
    img.insert(0, bg_line.clone());
    img.push(bg_line);
    for line in &mut img {
        line.insert(0, *bg);
        line.push(*bg);
    }

    let mut result = Vec::with_capacity(img.len());
    for i in 0..img.len() {
        let mut output_line = Vec::with_capacity(img[i].len());
        for j in 0..img[i].len() {
            output_line.push(alg[alg_index(&img, *bg, i, j)]);
        }
        result.push(output_line);
    }
    *bg = if *bg { alg[alg.len() - 1] } else { alg[0] };
    result
}

fn day20a(content: &str) -> usize {
    let (alg, img) = parse(content);
    let mut bg = false;
    lit_pixels(&apply_alg(apply_alg(img, &mut bg, &alg), &mut bg, &alg))
}

fn day20b(content: &str) -> usize {
    let (alg, mut img) = parse(content);
    let mut bg = false;
    for _ in 0..50 {
        img = apply_alg(img, &mut bg, &alg);
    }
    lit_pixels(&img)
}

fn main() {
    let content = include_str!("input20a.txt");
    println!("day20a: {}", day20a(content));
    println!("day20b: {}", day20b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let content = include_str!("test20a.txt");
        let (alg, img) = parse(content);

        assert_eq!(512, alg.len());
        assert_eq!(5, img.len());
        for line in img {
            assert_eq!(5, line.len());
        }
    }

    #[test]
    fn test_alg_index() {
        let content = include_str!("test20a.txt");
        let (_, img) = parse(content);
        assert_eq!(34, alg_index(&img, false, 2, 2));
        assert_eq!(0b000010010, alg_index(&img, false, 0, 0));
        assert_eq!(0b000100100, alg_index(&img, false, 0, 1));
        assert_eq!(0b000110000, alg_index(&img, false, 4, 4));
    }

    #[test]
    fn test_lit_pixels() {
        let content = include_str!("test20a.txt");
        let (_, img) = parse(content);
        assert_eq!(10, lit_pixels(&img));
    }

    #[test]
    fn test_apply_alg() {
        let content = include_str!("test20a.txt");
        let (alg, img) = parse(content);
        let mut bg = false;
        let img = apply_alg(img, &mut bg, &alg);
        assert_eq!(24, lit_pixels(&img));
        let img = apply_alg(img, &mut bg, &alg);
        assert_eq!(35, lit_pixels(&img));
    }

    #[test]
    fn test_day20b() {
        let content = include_str!("test20a.txt");
        assert_eq!(3351, day20b(content));
    }
}
