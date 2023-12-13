import scala.io.Source
import pprint.pprintln


case class Record(pattern: String, groups: IndexedSeq[Int])


def parseLine(s: String): Record  = {
  val Array(pattern, groups) = s.split(' ')
  Record(pattern, groups.split(',').map(_.toInt).toIndexedSeq)
}

def testRecord(r: Record): Boolean = {
  r.groups == r.pattern
    .split('.')
    .filterNot(_.isBlank)
    .map(_.length)
    .toVector
}

def countSolutions(r: Record): Int = {
  val totalDamaged = r.groups.sum
  val knownDamaged = r.pattern.count(_ == '#')
  val unknownDamaged = totalDamaged - knownDamaged
  val unknownIndices = r.pattern.zipWithIndex.filter(_._1 == '?').map(_._2)
  // pprintln(totalDamaged)
  // pprintln(knownDamaged)
  // pprintln(unknownDamaged)
  // pprintln(unknownIndices)

  val combinations = unknownIndices.combinations(unknownDamaged)
  // pprintln(combinations.toList)

  val possibleCombinations = combinations
    .map(combo => combo.foldLeft(r.pattern)((s, i) => s.updated(i, '#')).replaceAll("\\?", "\\."))
    .filter(e => testRecord(Record(e, r.groups)))
    .length

  possibleCombinations
}




def part1() = {
  val input = Source
    .fromFile("./input/day12.txt")
    .getLines
    .filterNot(_.isBlank)
    // .toList

  // pprintln(input)

  val result = input
    .map(parseLine)
    // .take(2)
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
