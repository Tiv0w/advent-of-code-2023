import scala.io.Source
import pprint.pprintln


case class Record(pattern: String, groups: IndexedSeq[Int])


def parseLine(s: String): Record  = {
  val Array(pattern, groups) = s.split(' ')
  Record(pattern, groups.split(',').map(_.toInt).toIndexedSeq)
}

def evaluateRecord(r: Record): Int = {
  val condition = r.groups == r.pattern
    .split('.')
    .filterNot(_.isBlank)
    .map(_.length)
    .toVector
  if (condition) 1 else 0
}

def countSolutions(r: Record): Int = {
  if (r.pattern.count(_ == '?') == 0) {
    // pprintln(r.pattern)
    // pprintln(testRecord(r))
    evaluateRecord(r)
  } else {
    val operational = r.pattern.replaceFirst("\\?", "\\.")
    val operationalCount = countSolutions(Record(operational, r.groups))
    if (r.groups.sum == r.pattern.count(_ == '#')) {
      operationalCount
    } else {
      val damaged = r.pattern.replaceFirst("\\?", "#")
      val damagedCount = countSolutions(Record(damaged, r.groups))
      operationalCount + damagedCount
    }
  }
}


def part1() = {
  val input = Source
    .fromFile("./input/day12.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  // pprintln(input)

  val result = input
    .map(parseLine)
    // .take(5)
    // .drop(1)
    .map(countSolutions)
    .sum

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
