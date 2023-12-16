import scala.io.Source
import pprint.pprintln
import scala.util.chaining._


implicit class PartitionByOnString[A](xs: String) {
  def partitionBy[B](f: Char => B): Vector[String] = {
    if (xs.isEmpty) {
      Vector()
    } else {
      var allVectors: Vector[String] = Vector()
      var currentValue = f(xs.head)
      var currentVector: Vector[Char] = Vector(xs.head)
      for (element <- xs.tail) {
        val elementValue = f(element)
        if (elementValue == currentValue) {
          currentVector = currentVector :+ element
        } else {
          allVectors = allVectors :+ currentVector.mkString
          currentValue = elementValue
          currentVector = Vector(element)
        }
      }
      allVectors :+ currentVector.mkString
    }
  }
}


def slideLineWest(line: String): String =
  line
    .partitionBy(_ == '#')
    .map(_.sorted.reverse)
    .mkString

def slidePlatformNorth(platform: Vector[String]): Vector[String] =
  platform
    .transpose
    .map(l => slideLineWest(l.mkString))
    .transpose
    .map(_.mkString)

def computeLoad(platform: Vector[String]): Long =
  platform
    .zipWithIndex
    .map{ case (s, idx) => (s, platform.length - idx) }
    .map{ case (s, load) => s.count(_ == 'O').toLong * load }
    .sum


def part1() = {
  val input = Source
    .fromFile("./input/day14.txt")
    .getLines
    .filterNot(_.isBlank)
    .toVector

  // println(input.mkString("\n"))
  // println()

  val slided = slidePlatformNorth(input)
  // println(slided.mkString("\n"))
  // println()
  pprintln(computeLoad(slided))
}

part1()

def printPlatform(platform: Vector[String]): Unit = {
  println(platform.mkString("\n"))
  println()
}

def slidePlatformWest(platform: Vector[String]): Vector[String] =
  platform
    .transpose.map(_.mkString)
    .pipe(slidePlatformNorth)
    .transpose.map(_.mkString)

def slidePlatformSouth(platform: Vector[String]): Vector[String] =
  platform
    .reverse
    .pipe(slidePlatformNorth)
    .reverse

def slidePlatformEast(platform: Vector[String]): Vector[String] =
  platform
    .transpose.map(_.mkString)
    .reverse
    .pipe(slidePlatformNorth)
    .reverse
    .transpose.map(_.mkString)


def spinCycle(platform: Vector[String]): Vector[String] =
  platform
    .pipe(slidePlatformNorth)
    .pipe(slidePlatformWest)
    .pipe(slidePlatformSouth)
    .pipe(slidePlatformEast)


def part2() = {
  val input = Source
    .fromFile("./examples/day14.txt")
    .getLines
    .filterNot(_.isBlank)
    .toVector

  // println(input.mkString("\n"))
  // println()

  val slided = Range(0, 10000).foldLeft(input)((prev, _) => spinCycle(prev))
  // val slided = spinCycle(input)
  // println(slided.mkString("\n"))
  // println()
  pprintln(computeLoad(slided))
}

part2()
