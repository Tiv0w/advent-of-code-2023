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

  pprintln(res)
}

part1()


def countScratchcards(xs: List[(Int, Int)]): Int = {
  xs match {
    case Nil => 0
    case (matches, copies) :: next => {
      val newNext = next.patch(0, next.take(matches).map(x => (x._1, x._2 + copies)), matches)
      copies + countScratchcards(newNext)
    }
  }
}

def part2() = {
  val input = Source
    .fromFile("./day4-input.txt")
    .getLines
    .filter(!_.isBlank())
    .map(parseLine)

  val preprocessed = input.map(x => (x._1.intersect(x._2).size, 1)).toList
  val result = countScratchcards(preprocessed)

  pprintln(result)
}

part2()
