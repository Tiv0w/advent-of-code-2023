import scala.io.Source


def part1() = {
  val all = Source
    .fromFile("./day1-input.txt")
    .getLines
    .map(_.toList.filter(_.isDigit).map(_.asDigit))
    .map((x) => x.head * 10 + x.last)
    .toList

  println(s"Part 1: ${all.sum}")
}

part1()


def part2() = {
  val digitsAsWords = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  val all = Source
    .fromFile("./day1-input.txt")
    .getLines
    .map(
      _.tails.flatMap(x =>
        if (x.size > 0 && x.head.isDigit)
          Some(x.head.asDigit)
        else
          digitsAsWords.find(y => x.startsWith(y)) match {
            case Some(i) => Some(digitsAsWords.indexOf(i))
            case None => None
          }
      ).toList
    )
    .map((x) => x.head * 10 + x.last)
    .toList

  println(s"Part 2: ${all.sum}")
}

part2()
