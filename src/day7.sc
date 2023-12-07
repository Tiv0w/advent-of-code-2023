import scala.io.Source
import pprint.pprintln


enum HandType {
  case Invalid, HighCard, OnePair, DoublePair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
}


def determineHandTypeAndStrength(hand: String): (HandType, Int) = {
  val occurences = hand
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .values
    .toList
    .sorted(Ordering[Int].reverse)

  val handType = occurences match {
    case List(5)             => HandType.FiveOfAKind
    case List(4, 1)          => HandType.FourOfAKind
    case List(3, 2)          => HandType.FullHouse
    case List(3, 1, 1)       => HandType.ThreeOfAKind
    case List(2, 2, 1)       => HandType.DoublePair
    case List(2, 1, 1, 1)    => HandType.OnePair
    case List(1, 1, 1, 1, 1) => HandType.HighCard
    case List(_*)            => HandType.Invalid
  }

  val strengthMap = Map[Char, Int](
    'T' -> 10,
    'J' -> 11,
    'Q' -> 12,
    'K' -> 13,
    'A' -> 14,
  ).withDefault(_.asDigit)

  val handStrength = hand
    .map(strengthMap(_))
    .reverse
    .zipWithIndex
    .map(x => scala.math.pow(15, x._2).toInt * x._1)
    .sum

  (handType, handStrength)
}

def part1() = {
  val input = Source
    .fromFile("./input/day7.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val result = input
    // .take(4)
    .map(line => (line.split(' ')(0), line.split(' ')(1).toInt))
    .map(handBid => {
      val (handType, handStrength) = determineHandTypeAndStrength(handBid._1)
      (handType, handStrength, handBid._2)
    })
    .sortBy(e => (e._1.ordinal, e._2))
    .map(_._3)
    .zipWithIndex
    .map(e => e._1 * (e._2 + 1))

  // pprintln(result)
  pprintln(result.sum)

}

part1()


def determineHandTypeAndStrengthPart2(hand: String): (HandType, Int) = {
  val occurencesMap = hand
    .groupMapReduce(identity)(_ => 1)(_ + _)

  val jokers = occurencesMap.getOrElse('J', 0)

  val occurences = occurencesMap
    .removed('J')
    .values
    .toList
    .sorted(Ordering[Int].reverse)
    match { // Prevents problem with a hand full of jokers
      case Nil => List(0)
      case value => value
    }

  val occurencesWithJoker = occurences.updated(0, occurences(0) + jokers)

  val handType = occurencesWithJoker match {
    case List(5)             => HandType.FiveOfAKind
    case List(4, 1)          => HandType.FourOfAKind
    case List(3, 2)          => HandType.FullHouse
    case List(3, 1, 1)       => HandType.ThreeOfAKind
    case List(2, 2, 1)       => HandType.DoublePair
    case List(2, 1, 1, 1)    => HandType.OnePair
    case List(1, 1, 1, 1, 1) => HandType.HighCard
    case List(_*)            => HandType.Invalid
  }

  val strengthMap = Map[Char, Int](
    'T' -> 10,
    'J' -> 1,
    'Q' -> 12,
    'K' -> 13,
    'A' -> 14,
  ).withDefault(_.asDigit)

  val handStrength = hand
    .map(strengthMap(_))
    .reverse
    .zipWithIndex
    .map(x => scala.math.pow(15, x._2).toInt * x._1)
    .sum

  (handType, handStrength)
}

def part2() = {
  val input = Source
    .fromFile("./input/day7.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val result = input
    .map(line => (line.split(' ')(0), line.split(' ')(1).toInt))
    .map(handBid => {
      val (handType, handStrength) = determineHandTypeAndStrengthPart2(handBid._1)
      (handType, handStrength, handBid._2)
    })
    .sortBy(e => (e._1.ordinal, e._2))
    .map(_._3)
    .zipWithIndex
    .map(e => e._1 * (e._2 + 1))

  pprintln(result.sum)
}

part2()
