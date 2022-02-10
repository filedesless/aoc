use std::collections::VecDeque;

type BitStream = VecDeque<bool>;

type Version = usize;
type TypeId = usize;
type Value = usize;

#[derive(Debug, Eq, PartialEq)]
enum Packet {
    Literal(Version, TypeId, Value),
    Operator(Version, TypeId, Vec<Packet>),
}

use Packet::*;

fn parse(content: &str) -> BitStream {
    let mut bits = VecDeque::new();
    for nibble in content.chars() {
        let n = nibble.to_digit(16).unwrap();
        for &bit in &[
            n & 0b1000 != 0,
            n & 0b0100 != 0,
            n & 0b0010 != 0,
            n & 0b0001 != 0,
        ] {
            bits.push_back(bit);
        }
    }
    bits
}

fn read_number(bits: &BitStream) -> usize {
    let mut v = 1;
    let mut s = 0;
    for &bit in bits.iter().rev() {
        if bit {
            s += v;
        }
        v *= 2;
    }
    s
}

fn read_packet(mut bits: BitStream) -> (Option<Packet>, BitStream) {
    let mut drain = bits.drain(0..3).collect();
    let version = read_number(&drain);
    drain = bits.drain(0..3).collect();
    let type_id = read_number(&drain);

    if type_id == 4 {
        let mut lit = VecDeque::default();
        loop {
            drain = bits.drain(0..5).collect();
            let last = !drain.pop_front().unwrap();
            lit.extend(drain);
            if last {
                break;
            }
        }
        (Some(Literal(version, type_id, read_number(&lit))), bits)
    } else {
        let length_type_id = bits.pop_front().unwrap();
        let mut subpackets = vec![];
        if length_type_id {
            drain = bits.drain(0..11).collect();
            let sub_count = read_number(&drain);
            loop {
                let (packet, rest) = read_packet(bits);
                if let Some(p) = packet {
                    subpackets.push(p);
                } else {
                    return (None, rest);
                }
                bits = rest;
                if bits.iter().all(|&b| !b) || subpackets.len() >= sub_count {
                    break;
                }
            }
        } else {
            drain = bits.drain(0..15).collect();
            let sub_len = read_number(&drain);
            drain = bits.drain(0..sub_len).collect();
            loop {
                let (packet, rest) = read_packet(drain);
                if let Some(p) = packet {
                    subpackets.push(p);
                } else {
                    return (None, rest);
                }
                drain = rest;
                if drain.iter().all(|&b| !b) {
                    break;
                }
            }
        }
        (Some(Operator(version, type_id, subpackets)), bits)
    }
}

fn version(packet: &Packet) -> usize {
    match packet {
        Literal(v, _, _) => *v,
        Operator(v, _, children) => *v + children.iter().map(version).sum::<usize>(),
    }
}

fn evaluate(packet: &Packet) -> usize {
    match packet {
        Literal(_, _, value) => *value,
        Operator(_, 0, children) => children.iter().map(evaluate).sum(),
        Operator(_, 1, children) => children.iter().map(evaluate).product(),
        Operator(_, 2, children) => children.iter().map(evaluate).min().unwrap(),
        Operator(_, 3, children) => children.iter().map(evaluate).max().unwrap(),
        Operator(_, 5, children) => {
            if evaluate(&children[0]) > evaluate(&children[1]) {
                1
            } else {
                0
            }
        }
        Operator(_, 6, children) => {
            if evaluate(&children[0]) < evaluate(&children[1]) {
                1
            } else {
                0
            }
        }
        Operator(_, 7, children) => {
            if evaluate(&children[0]) == evaluate(&children[1]) {
                1
            } else {
                0
            }
        }
        _ => panic!("unrecognized packet: {:?}", packet),
    }
}

fn day16a(content: &str) -> usize {
    let (packet, _) = read_packet(parse(content));
    version(&packet.unwrap())
}

fn day16b(content: &str) -> usize {
    let (packet, _) = read_packet(parse(content));
    evaluate(&packet.unwrap())
}

fn main() {
    let content = include_str!("day16a.txt");
    println!("day16a: {}", day16a(content));
    println!("day16b: {}", day16b(content));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(24, parse("D2FE28").len());
        assert_eq!(56, parse("38006F45291200").len());
    }

    #[test]
    fn test_read_number() {
        assert_eq!(6, read_number(&[true, true, false].into()));
        assert_eq!(4, read_number(&[true, false, false].into()));
        assert_eq!(
            2021,
            read_number(
                &[false, true, true, true, true, true, true, false, false, true, false, true]
                    .into()
            )
        );
    }

    #[test]
    fn test_read_packet() {
        let (packet, _) = read_packet(parse("D2FE28"));
        assert_eq!(Some(Literal(6, 4, 2021)), packet);
        let (packet, _) = read_packet(parse("38006F45291200"));
        assert_eq!(
            Some(Operator(1, 6, vec![Literal(6, 4, 10), Literal(2, 4, 20),])),
            packet
        );
        let (packet, _) = read_packet(parse("EE00D40C823060"));
        assert_eq!(
            Some(Operator(
                7,
                3,
                vec![Literal(2, 4, 1), Literal(4, 4, 2), Literal(1, 4, 3),]
            )),
            packet
        );
    }

    #[test]
    fn test_day16a() {
        assert_eq!(16, day16a("8A004A801A8002F478"));
        assert_eq!(12, day16a("620080001611562C8802118E34"));
        assert_eq!(23, day16a("C0015000016115A2E0802F182340"));
        assert_eq!(31, day16a("A0016C880162017C3686B18A3D4780"));
    }

    #[test]
    fn test_day16b() {
        assert_eq!(3, day16b("C200B40A82"));
        assert_eq!(54, day16b("04005AC33890"));
        assert_eq!(7, day16b("880086C3E88112"));
        assert_eq!(9, day16b("CE00C43D881120"));
        assert_eq!(1, day16b("D8005AC2A8F0"));
        assert_eq!(0, day16b("F600BC2D8F"));
        assert_eq!(0, day16b("9C005AC2F8F0"));
        assert_eq!(1, day16b("9C0141080250320F1802104A08"));
    }
}
