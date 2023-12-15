import scala.io.Source
import pprint.pprintln
import $ivy.`dev.optics::monocle-core:3.1.0`, monocle.syntax.all._


def evaluatePatternOneWay(pattern: Vector[String]): Vector[Int] =
  pattern
    .indices
    .tail
    .map(idx => {
      val (top, bottom) = pattern.splitAt(idx)
      (idx, top.reverse.zip(bottom))
    })
    .filter(_._2.forall(pair => pair._1 == pair._2))
    .map(_._1)
    .toVector

def evaluatePattern(pattern: Vector[String]): Int = {
  val horizontal = evaluatePatternOneWay(pattern)
    .headOption
    .getOrElse(0)
  val vertical = evaluatePatternOneWay(pattern.transpose.map(_.mkString))
    .headOption
    .getOrElse(0)

  horizontal * 100 + vertical
}


def part1() = {
  val input = Source
    .fromFile("./input/day13.txt")
    .mkString
    .split("\n\n")
    .map(_.split('\n').toVector)
    .toVector

  val result = input
    .map(evaluatePattern)
    .sum

  pprintln(result)
}

part1()


def oppositeChar(c: Char): Char = if (c == '#') '.' else '#'

def changePattern(pattern: Vector[String], x: Int, y: Int): Vector[String] = {
  pattern.focus(_.index(y).index(x)).modify(oppositeChar)
}

def repairAndEvaluatePattern(pattern: Vector[String]): Int = {
  val horizontal = evaluatePatternOneWay(pattern)
    .headOption
    .getOrElse(0)
  val vertical = evaluatePatternOneWay(pattern.transpose.map(_.mkString))
    .headOption
    .getOrElse(0)

  val otherPatterns = for
    y <- 0 until pattern.length
    x <- 0 until pattern(0).length
  yield changePattern(pattern, x, y)

  val newReflection = otherPatterns
    .map(p => (
      evaluatePatternOneWay(p)
        .find(_ != horizontal)
        .getOrElse(0),
      evaluatePatternOneWay(p.transpose.map(_.mkString))
        .find(_ != vertical)
        .getOrElse(0)
    ))
    .filter(_ != (0, 0))
    .head

  if (newReflection._1 == horizontal)
    newReflection._2
  else if (newReflection._2 == vertical)
    newReflection._1 * 100
  else
    newReflection._1 * 100 + newReflection._2
}


def part2() = {
  val input = Source
    .fromFile("./input/day13.txt")
    .mkString
    .split("\n\n")
    .map(_.split('\n').toVector)
    .toVector

  val result = input
    .map(repairAndEvaluatePattern)
    .sum

  pprintln(result)
}

part2()
