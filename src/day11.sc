import scala.io.Source
import pprint.pprintln


case class Coords(x: Int, y: Int)


def findGalaxies(universe: IndexedSeq[IndexedSeq[Char]]): IndexedSeq[Coords] =
  for
    x <- 0 until universe(0).length
    y <- 0 until universe.length
    if universe(y)(x) == '#'
  yield Coords(x, y)


def findExpandingRows(universe: IndexedSeq[IndexedSeq[Char]]): IndexedSeq[Int] =
  universe
    .zipWithIndex
    .filter(_._1.forall(_ == '.'))
    .map(_._2)


def distance(a: Coords, b: Coords) =
    (a.x - b.x).abs + (a.y - b.y).abs


def computeShortestPath(a: Coords, b: Coords, expandingRows: IndexedSeq[Int], expandingCols: IndexedSeq[Int], multiplier: Long): Long = {
  val rows = expandingRows.filter(y => (a.y to b.y).contains(y) || (b.y to a.y).contains(y))
  val cols = expandingCols.filter(x => (a.x to b.x).contains(x) || (b.x to a.x).contains(x))

  distance(a, b) + cols.length * (multiplier - 1) + rows.length * (multiplier - 1)
}


def sumGalaxiesDistances(universe: IndexedSeq[IndexedSeq[Char]], multiplier: Long): Long = {
  val expandingRows = findExpandingRows(universe)
  val expandingCols = findExpandingRows(universe.transpose)

  val galaxies: IndexedSeq[Coords] = findGalaxies(universe)

  galaxies
    .combinations(2)
    .map { case IndexedSeq(a, b) => computeShortestPath(a, b, expandingRows, expandingCols, multiplier) }
    .sum
}


def part1() = {
  val universe = Source
    .fromFile("./input/day11.txt")
    .getLines
    .filterNot(_.isBlank)
    .map(_.toIndexedSeq)
    .toIndexedSeq

  val result = sumGalaxiesDistances(universe, 2L)

  pprintln(result)
}

part1()


def part2() = {
  val universe = Source
    .fromFile("./input/day11.txt")
    .getLines
    .filterNot(_.isBlank)
    .map(_.toIndexedSeq)
    .toIndexedSeq

  val result = sumGalaxiesDistances(universe, 1_000_000L)

  pprintln(result)
}

part2()
