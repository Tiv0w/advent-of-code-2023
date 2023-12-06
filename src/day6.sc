import scala.io.Source
import pprint.pprintln


def parseInput(lines: List[String]): List[(Long, Long)] = {
  val List(times, records) = lines
    .map(
      _.split(':')(1).trim
        .split("\\s+")
        .map(_.trim.toLong)
        .toList)
  times.zip(records)
}

def waysToWinRace(race: (Long, Long)): Long = {
  val (time, record) = race
  (0L to time)
    .map(hold => (time - hold) * hold)
    .filter(_ > record)
    .length
}

def part1() = {
  val input = Source
    .fromFile("./input/day6.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val races = parseInput(input)

  pprintln(races.map(waysToWinRace).product)
}

part1()


def parseInputPart2(lines: List[String]): (Long, Long) = {
  val List(time, record) = lines.map(_.split(':')(1).trim.replaceAll(" ", "").toLong)
  (time, record)
}

def part2() = {
  val input = Source
    .fromFile("./input/day6.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val race = parseInputPart2(input)

  pprintln(waysToWinRace(race))
}

part2()
