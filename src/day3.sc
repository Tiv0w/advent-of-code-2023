import scala.io.Source
import pprint.pprintln


def getNeighboursIndices(width: Int)(n: Int): List[(Int, Int)] = {
  val neighbours = for {
    y <- 0 until 3
    x <- 0 until 3
    if (x != 1 || y != 1)
    if -1 < (x + n - 1) && (x + n - 1) < width
  } yield (x + n - 1, y)

  neighbours.toList
}

def findNumbers(line: String): List[(Int, Int)] = {
  var last: Option[Char] = None
  var numbers: List[(Int, Int)] = List.empty
  var currentNumber: (Int, Int) = (-1, -1)
  for (x <- 0 until line.length) {
    val char = line(x)
    if (char.isDigit) {
      last match {
        case None => currentNumber = (x, currentNumber._2)
        case Some(value) if !value.isDigit => currentNumber = (x, currentNumber._2)
        case Some(_) => None
      }
    } else {
      last match {
        case None => None
        case Some(value) if value.isDigit => {
          numbers = numbers :+ (currentNumber._1, x - 1)
          currentNumber = (-1, -1)
        }
        case Some(_) => None
      }
    }
    last = Some(char)
  }

  if (currentNumber._1 != -1 && last.get.isDigit) {
    numbers = numbers :+ (currentNumber._1, line.length - 1)
  } else if (currentNumber._1 != -1) {
    numbers = numbers :+ (currentNumber._1, line.length - 2)
  }

  numbers
}


def getPartNumbers(lines: List[String]): List[Int] = {
  val List(l1, l2, l3) = lines
  val numbers = findNumbers(l2)

  val numres = numbers
    .map(number =>
      (number, Range
        .inclusive(number._1, number._2)
        .flatMap(getNeighboursIndices(l1.length))
        .distinct
        .map(coords => lines(coords._2)(coords._1)))
    )
    .filter(number => number._2.exists(c => !c.isDigit && c != '.'))
    .map(_._1)
    .map(x => l2.slice(x._1, x._2 + 1).toInt)

  numres
}


def part1() = {
  val input = Source
    .fromFile("./input/day3.txt")
    .getLines
    .filter(!_.isBlank)
    .toList

  val width = input(0).length
  val result = input
    .prepended("." * width)
    .appended("." * width)
    .sliding(3)
    .flatMap(getPartNumbers)
    .toList

  // pprintln(result)
  pprintln(result.sum)
}

part1()



def getGearRatios(lines: List[String]): List[Int] = {
  val List(l1, l2, l3) = lines
  val numbers = findNumbers(l2)

  val numres = numbers
    .map(number =>
      (number, Range
        .inclusive(number._1, number._2)
        .flatMap(getNeighboursIndices(l1.length))
        .distinct
        .map(coords => lines(coords._2)(coords._1)))
    )
    .filter(number => number._2.exists(c => !c.isDigit && c != '.'))
    .map(_._1)
    .map(x => l2.slice(x._1, x._2 + 1).toInt)

  numres
}


def part2() = {
  val input = Source
    .fromFile("./examples/day3.txt")
    .getLines
    .filter(!_.isBlank)
    .toList

  val width = input(0).length
  val result = input
    .prepended("." * width)
    .appended("." * width)
    .sliding(3)
    .flatMap(getPartNumbers)
    .toList

  // pprintln(result)
  pprintln(result.sum)
}

part2()
