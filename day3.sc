import scala.io.Source
import scala.util.matching.Regex

def getBoundsOfNumbers(lines: List[String]): Unit = {

}

def part1() = {
  val input = Source
    .fromFile("./day3-input-example.txt")
    .getLines
    .filter(!_.isBlank)
    .toList

  val r = input
    .sliding(3)
    .toList

  for (i <- 0 until 3) {
    println(r(i))
  }
}

part1()

val pattern = """(\d|\.)""".r

val inp = Source
  .fromFile("./day3-input.txt")
  .getLines
  .mkString

val f = pattern.replaceAllIn(inp, x => "")

println(f.distinct)

// println(inp)
