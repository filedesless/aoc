from enum import IntEnum, auto
from collections import Counter


class Card:
    value: str

    def __init__(self, value: str):
        self.value = value

    def __eq__(self, other):
        return self.value == other.value

    def __hash__(self) -> int:
        return hash(self.value)

    def __lt__(self, other):
        l = "AKQJT98765432"
        return l.index(self.value) > l.index(other.value)

    def __repr__(self) -> str:
        return self.value


class HandType(IntEnum):
    HIGH = auto()
    ONE = auto()
    TWO = auto()
    THREE = auto()
    FULL = auto()
    FOUR = auto()
    FIVE = auto()


class Hand:
    cards: (Card, Card, Card, Card, Card)
    bid: int

    def __init__(self, cards: str, bid: int) -> None:
        self.cards = tuple(Card(card) for card in cards)
        self.bid = bid

    def __lt__(self, other):
        a, b = self.type(), other.type()
        if a == b:
            return self.cards < other.cards
        return a < b

    def type(self) -> HandType:
        c = Counter(self.cards)
        if 5 in c.values():
            return HandType.FIVE
        elif 4 in c.values():
            return HandType.FOUR
        elif 3 in c.values():
            if len(c) == 2:
                return HandType.FULL
            return HandType.THREE
        elif 2 in c.values():
            cc = Counter(c.values())
            if cc[2] == 2:
                return HandType.TWO
            return HandType.ONE
        return HandType.HIGH

    def __repr__(self) -> str:
        return "".join([str(card) for card in self.cards]) + " " + str(self.bid)


lines = [line.strip() for line in open("input", encoding="utf8").readlines()]
hands = [Hand(line.split()[0], int(line.split()[1])) for line in lines]
hands.sort()

print(sum(hand.bid * (i + 1) for (i, hand) in enumerate(sorted(hands))))