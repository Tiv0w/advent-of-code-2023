import scala.io.Source
import pprint.pprintln



case class Record(pattern: String, groups: Seq[Int])


def parseLine(s: String): Record  = {
  val Array(pattern, groups) = s.split(' ')
  Record(pattern, groups.split(',').map(_.toInt).toIndexedSeq)
}

def testPossibility(s: String, groups: Seq[Int]): Boolean = {
  s.split('.').map(_.length) == groups.toArray
}

def generatePossibilities(s: String, groups: Seq[Int]): Int = {
  if (s.count(_ == '?') == 0) {
    if (testPossibility(s, groups))
      1
    else
      0
  } else {
    val operational = s.replaceFirst("\\?", "\\.")
    val damaged = s.replaceFirst("\\?", "#")

    generatePossibilities(operational, groups) + generatePossibilities(damaged, groups)
  }
}


def countSolutions(record: Record): Int = {
  pprintln(record)

  val pattern = record.pattern

  pprintln(generatePossibilities(record.pattern, record.groups))


  pprintln(pattern)
  0
}

def part1() = {
  val input = Source
    .fromFile("./examples/day12.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  pprintln(input)

  val result = input
    .map(parseLine)
    .take(2)
    .map(countSolutions)

  pprintln(result)
}

part1()


// pprintln(
//   Source
//     .fromFile("./input/day12.txt")
//     .getLines
//     .filterNot(_.isBlank)
//     .map(_.split(' ')(0).count(_ == '?'))
//     .toVector
//     .groupBy(identity)
//     .mapValues(_.length)
//     // .max
// )
