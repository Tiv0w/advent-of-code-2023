import scala.io.Source
import pprint.pprintln


case class Coords(x: Int, y: Int)


def findGalaxies(universe: IndexedSeq[IndexedSeq[Char]]): IndexedSeq[Coords] = {
  for {
    x <- 0 until universe(0).length
    y <- 0 until universe.length
    if universe(y)(x) == '#'
  } yield Coords(x, y)
}


def part1() = {
  val input = Source
    .fromFile("./input/day11.txt")
    .getLines
    .filterNot(_.isBlank)
    .map(_.toIndexedSeq)
    .toIndexedSeq

  // pprintln(input)

  pprintln((input(0).length, input.length))

  val expandedUniverse: IndexedSeq[IndexedSeq[Char]] = input
    .flatMap(line => if (line.forall(_ == '.')) IndexedSeq(line, line) else IndexedSeq(line))
    .transpose
    .flatMap(line => if (line.forall(_ == '.')) IndexedSeq(line, line) else IndexedSeq(line))
    .transpose


  val galaxies: IndexedSeq[Coords] = findGalaxies(expandedUniverse)

  pprintln(galaxies.length)

  // pprintln(expandedUniverse)

  pprintln((expandedUniverse(0).length, expandedUniverse.length))

  // pprintln(galaxies)

  val result = galaxies
    .combinations(2)
    .map { case IndexedSeq(a, b) => (a.x - b.x).abs + (a.y - b.y).abs }
    // .toList

  pprintln(result.sum)
}

part1()


def findExpandingRows(universe: IndexedSeq[IndexedSeq[Char]]): IndexedSeq[Int] =
  universe
    .zipWithIndex
    .filter(_._1.forall(_ == '.'))
    .map(_._2)


def computeShortestPath(a: Coords, b: Coords, expandingRows: IndexedSeq[Int], expandingCols: IndexedSeq[Int], multiplier: Long): Long = {
  val rows = expandingRows.filter(y => (a.y to b.y).contains(y) || (b.y to a.y).contains(y))
  val cols = expandingCols.filter(x => (a.x to b.x).contains(x) || (b.x to a.x).contains(x))

  (
    (a.x - b.x).abs +
      (a.y - b.y).abs +
      cols.length.toLong * (multiplier - 1) +
      rows.length.toLong * (multiplier - 1)
  )
}


def part2() = {
  val input = Source
    .fromFile("./input/day11.txt")
    .getLines
    .filterNot(_.isBlank)
    .map(_.toIndexedSeq)
    .toIndexedSeq

  val universe = input

  // pprintln(input)

  pprintln((input(0).length, input.length))

  val expandingRows = findExpandingRows(universe)
  val expandingCols = findExpandingRows(universe.transpose)

  // val expandedUniverse: IndexedSeq[IndexedSeq[Char]] = input
  //   .flatMap(line => if (line.forall(_ == '.')) IndexedSeq.fill(1_000)(line) else IndexedSeq(line))
  //   .transpose
  //   .flatMap(line => if (line.forall(_ == '.')) IndexedSeq.fill(1_000)(line) else IndexedSeq(line))
  //   .transpose


  val galaxies: IndexedSeq[Coords] = findGalaxies(universe)

  pprintln(galaxies.length)

  // pprintln(expandedUniverse)

  // pprintln((expandedUniverse(0).length, expandedUniverse.length))

  // pprintln(galaxies)

  val result = galaxies
    .combinations(2)
    .map { case IndexedSeq(a, b) => computeShortestPath(a, b, expandingRows, expandingCols, 1_000_000L) }
    // .toList

  pprintln(result.sum)
}

println()
part2()
