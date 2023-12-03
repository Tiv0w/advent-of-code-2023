import scala.io.Source


def parseGrab(g: String): (Int, Int, Int) = {
  val parts = g.split(", ")
  val find = (color: String) => parts
    .find(_.endsWith(color))
    .getOrElse(s"0 ${color}")
    .split(" ")(0)
    .toInt
  return (find("red"), find("green"), find("blue"))
}


def parseLine(s: String): (Int, List[(Int, Int, Int)]) = {
  val gameNumber = s.split(": ")(0).split(" ")(1).toInt
  val grabs = s.split(": ")(1).split("; ").map(parseGrab).toList
  return (gameNumber, grabs)
}

def computeMinimumBalls(game: (Int, List[(Int, Int, Int)])) = {
  val (gameNumber, l) = game
  val (t1, t2, t3) = l.unzip3
  (gameNumber, List(t1, t2, t3).map(_.max))
}


def part1() = {
  val input = Source
    .fromFile("./day2-input.txt")
    .getLines
    .filter(!_.isBlank())
    .map(parseLine)
    .map(computeMinimumBalls)
    .map(x => (x._1, x._2.zip(List(12, 13, 14))))
    .filter(_._2.forall(x => x._1 <= x._2))
    .map(_._1)
    .sum
    // .toList
    // .map(_.map(_.max))

  // println(input(0))
  // println(input(1))
  println(input)
}


part1()


def part2() = {
  val input = Source
    .fromFile("./day2-input.txt")
    .getLines
    .filter(!_.isBlank())
    .map(parseLine)
    .map(computeMinimumBalls)
    .map(x => x._2.product)
    .sum

  // println(input(0))
  // println(input(1))
  println(input)
}

part2()
