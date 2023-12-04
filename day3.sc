import scala.io.Source
import scala.util.matching.Regex


def partitionBy[A, B](xs: Seq[A], f: A => B): Seq[Seq[A]] = {
  if (xs.isEmpty) {
    Seq()
  } else {
    var allSeqs: Seq[Seq[A]] = Seq()
    var currentValue = f(xs.head)
    var currentSeq: Seq[A] = Seq(xs.head)
    for (element <- xs.tail) {
      val elementValue = f(element)
      if (elementValue == currentValue) {
        currentSeq = currentSeq :+ element
      } else {
        allSeqs = allSeqs :+ currentSeq
        currentValue = elementValue
        currentSeq = Seq(element)
      }
    }
    allSeqs :+ currentSeq
  }
}

def getBoundsOfNumbers(lines: List[String]): Unit = {
  println(lines)
  val l = partitionBy(lines(0).zipWithIndex, _._1.isDigit)
  println(l)
  return lines.map("""\d+""".r.split(_))
}

def part1() = {
  val input = Source
    .fromFile("./day3-input-example.txt")
    .getLines
    .filter(!_.isBlank)
    .toList

  val r = input
    .sliding(3)
    .take(1)
    .map(getBoundsOfNumbers(_))
    .toList

  // for (i <- 0 until 3) {
  //   println(r(i))
  // }
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
