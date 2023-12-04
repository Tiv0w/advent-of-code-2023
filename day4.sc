import scala.io.Source
import pprint.pprintln


def parseLine(s: String): (List[Int], List[Int]) = {
  val Array(winning, ours) = s
    .split(": ")(1)
    .split('|')
    .map(_.strip.split("\\s+").map(_.toInt).toList)
  (winning, ours)
}

def part1() = {
  val input = Source
    .fromFile("./day4-input.txt")
    .getLines
    .filter(!_.isBlank())
    .map(parseLine)

  val res = input
    .map(xs => xs._1.intersect(xs._2))
    .map(xs => scala.math.pow(2, xs.size - 1).intValue)
    .sum
    // .toList

  pprintln(res)
}

part1()


def part2() = {
  val input = Source
    .fromFile("./day4-input-example.txt")
    .getLines
    .filter(!_.isBlank())
    .map(parseLine)

  val res = input
    .zipWithIndex
    .map(x => (x._1, x._2, 1))
    // .(x => x._1._1.intersect(x._1._2))
    // .map(xs => scala.math.pow(2, xs.size - 1).intValue)
    // .sum
    .toList

  pprintln(res)
}

part2()
