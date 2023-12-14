import scala.io.Source
import pprint.pprintln


def evaluatePatternOneWay(pattern: Vector[String]): Int =
  pattern
    .indices
    .tail
    .map(idx => {
      val (top, bottom) = pattern.splitAt(idx)
      (idx, top.reverse.zip(bottom))
    })
    .find(_._2.forall(pair => pair._1 == pair._2))
    .getOrElse((0, 0))
    ._1


def evaluatePattern(pattern: Vector[String]): Int = {
  val horizontal = evaluatePatternOneWay(pattern)
  val vertical = evaluatePatternOneWay(pattern.transpose.map(_.mkString))

  horizontal * 100 + vertical
}


def part1() = {
  val input = Source
    .fromFile("./input/day13.txt")
    .mkString
    .split("\n\n")
    .map(_.split('\n').toVector)
    .toVector
    // .getLines
    // .mkString
    // .getLines
  // .filterNot(_.isBlank)
    // .toList

  // pprintln(input)

  val result = input
    // .take(2)
    // .drop(1)
    .map(evaluatePattern)
    .sum

  pprintln(result)
}

part1()
