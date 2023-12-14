import scala.io.Source
import pprint.pprintln
// import $ivy.`com.softwaremill.quicklens::quicklens:1.9.6`, com.softwaremill.quicklens._
import $ivy.`dev.optics::monocle-core:3.1.0`, monocle.syntax.all._
import java.util.NoSuchElementException


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



def oppositeChar(c: Char): Char = if (c == '#') '.' else '#'

def changePattern(pattern: Vector[String], x: Int, y: Int): Vector[String] = {
  pattern.focus(_.index(y).index(x)).modify(oppositeChar)
}

def repairAndEvaluatePattern(pattern: Vector[String]): Int = {
  val horizontal = evaluatePatternOneWay(pattern)
  val vertical = evaluatePatternOneWay(pattern.transpose.map(_.mkString))

  // pprintln("hello")
  // pprintln((horizontal, vertical))
  val otherPatterns = for
    y <- 0 until pattern.length
    x <- 0 until pattern(0).length
  yield changePattern(pattern, x, y)

  // var newReflection: (Int, Int, Vector[String])
  // try {
  val newReflections = otherPatterns
       .map(p => (evaluatePatternOneWay(p), evaluatePatternOneWay(p.transpose.map(_.mkString)), p))
       .filter(x => (x._1, x._2) != (0, 0))
       .filter(x => (x._1, x._2) != (horizontal, vertical))
       // .head// Option
      // .getOrElse((0, 0, 0))
  // } catch {
    // case nse: NoSuchElementException =>
  // }

  if (newReflections.length == 0) {
    println(pattern.mkString("\n"))
    pprintln((horizontal, vertical))
    val changed = changePattern(pattern, 9, 15)
    println(changed.mkString("\n"))
    println()
    println(changed.transpose.map(_.mkString).mkString("\n"))
    pprintln(evaluatePatternOneWay(changed.transpose.map(_.mkString)))
  }

  // pprintln(newReflections)

  val newReflection = newReflections.head
  // pprintln((newReflection._1, newReflection._2))
  // println(newReflection._3.mkString("\n"))

  if (newReflection._1 == horizontal)
    newReflection._2
  else if (newReflection._2 == vertical)
    newReflection._1 * 100
  else
    newReflection._1 * 100 + newReflection._2
  // horizontal * 100 + vertical
}



def part2() = {
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
    .map(repairAndEvaluatePattern)
    .sum

  pprintln(result)
}

part2()
