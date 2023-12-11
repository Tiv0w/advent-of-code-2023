import scala.io.Source
import pprint.pprintln



def parseHistory(s: String): List[Int] = {
  s.split(' ').map(_.toInt).toList
}

def nextValue(history: List[Int]): Int = {
  if (history.forall(_ == 0)) {
    0
  } else {
    val diffs = history.sliding(2).map(_.reduce((a, b) => b - a)).toList
    val nextDiff = nextValue(diffs)
    history.last + nextDiff
  }
}


def part1() = {
  val input = Source
    .fromFile("./input/day9.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val result = input
    .map(parseHistory)
    .map(nextValue)

  pprintln(result.sum)
}

part1()


def previousValue(history: List[Int]): Int = {
  if (history.forall(_ == 0)) {
    0
  } else {
    val diffs = history.sliding(2).map(_.reduce((a, b) => b - a)).toList
    val previousDiff = previousValue(diffs)
    history.head - previousDiff
  }
}


def part2() = {
  val input = Source
    .fromFile("./input/day9.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val result = input
    .map(parseHistory)
    .map(previousValue)

  pprintln(result.sum)
}

part2()
