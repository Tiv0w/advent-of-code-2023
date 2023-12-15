import scala.io.Source
import pprint.pprintln



def slideLineWest(line: String): String = {
  val f = (line :+ '.').split('#')// .map(_.sorted.reverse)
  pprintln(f)
  pprintln(f.mkString("#"))
  f.mkString("#")
}

def slidePlatformNorth(p: Vector[String]): Vector[String] = {
  val result = p
    .transpose
    .map(l => slideLineWest(l.mkString))
    .transpose
    .map(_.mkString)

  result
}


def part1() = {
  val input = Source
    .fromFile("./examples/day14.txt")
    .getLines
    .filterNot(_.isBlank)
    .toVector

  println(input.mkString("\n"))

  slidePlatformNorth(input)
}

part1()
