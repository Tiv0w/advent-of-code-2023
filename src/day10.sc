import scala.io.Source
import pprint.pprintln


def part1() = {
  val input = Source
    .fromFile("./examples/day10-1.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  pprintln(input)
}

part1()
