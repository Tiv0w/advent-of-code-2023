import scala.io.Source
import pprint.pprintln
import scala.collection.immutable.Set


enum Direction:
  case Right, Down, Left, Up
// case class Coords(x: Int, y: Int)
case class Beam(x: Int, y: Int, dir: Direction)


def readInput(s: String) = Source.fromFile(s).getLines.filterNot(_.isBlank).toVector

def nextBeam(beam: Beam) = beam.dir match {
  case Direction.Right => beam.copy(x = beam.x + 1)
  case Direction.Left => beam.copy(x = beam.x - 1)
  case Direction.Up => beam.copy(y = beam.y - 1)
  case Direction.Down => beam.copy(y = beam.y + 1)
}

def followBeams(grid: Vector[String], beams: Vector[Beam], cache: Set[Beam]): Int = {
  val n = beams.flatMap(beam => {
    val currentTile = grid(beam.y)(beam.x)
    // pprintln(currentTile)
    val next = (currentTile, beam.dir) match {
      case ('.', _) => Vector(nextBeam(beam))
      case ('|', Direction.Down | Direction.Up) => Vector(nextBeam(beam))
      case ('-', Direction.Right | Direction.Left) => Vector(nextBeam(beam))
      case ('|', Direction.Right | Direction.Left) =>
        Vector(nextBeam(beam.copy(dir = Direction.Up)), nextBeam(beam.copy(dir = Direction.Down)))
      case ('-', Direction.Up | Direction.Down) =>
        Vector(nextBeam(beam.copy(dir = Direction.Right)), nextBeam(beam.copy(dir = Direction.Left)))
      case ('\\', Direction.Up) => Vector(nextBeam(beam.copy(dir = Direction.Left)))
      case ('\\', Direction.Down) => Vector(nextBeam(beam.copy(dir = Direction.Right)))
      case ('\\', Direction.Left) => Vector(nextBeam(beam.copy(dir = Direction.Up)))
      case ('\\', Direction.Right) => Vector(nextBeam(beam.copy(dir = Direction.Down)))
      case ('/', Direction.Up) => Vector(nextBeam(beam.copy(dir = Direction.Right)))
      case ('/', Direction.Down) => Vector(nextBeam(beam.copy(dir = Direction.Left)))
      case ('/', Direction.Left) => Vector(nextBeam(beam.copy(dir = Direction.Down)))
      case ('/', Direction.Right) => Vector(nextBeam(beam.copy(dir = Direction.Up)))
      case (_, _) => Vector(nextBeam(beam))
    }
    // pprintln(next)
    next
  })
  val width = grid(0).length
  val height = grid.length
  // pprintln(n)
  val nextBeams = n
    .filter(beam => 0 <= beam.x && beam.x < width && 0 <= beam.y && beam.y < height)
    .filterNot(beam => cache.contains(beam))
  val nextCache = cache.concat(nextBeams)
  // pprintln(newCache)
  if (nextBeams.isEmpty)
    nextCache.toVector.distinctBy(beam => (beam.x, beam.y)).length
  else
    followBeams(grid, nextBeams, nextCache)
}


def part1() = {
  val input = readInput("./input/day16.txt")
  // println(input.mkString("\n"))

  val initialBeam = Beam(0, 0, Direction.Right)
  // Range(0, 10).foldLeft()((beams, _) => followBeams(input, beams))
  // var result = followBeams(input, )
  // result = followBeams(input, result)
  // result = followBeams(input, result)

  val energized = followBeams(input, Vector(initialBeam), Set(initialBeam))
  pprintln(energized)
}

part1()


def part2() = {
  val input = readInput("./input/day16.txt")
  // println(input.mkString("\n"))

  val width = input(0).length
  val height = input.length

  val allInitialBeams = Vector(
    for (x <- 0 until (width - 1)) yield Beam(x, 0, Direction.Down),
    for (x <- 0 until (width - 1)) yield Beam(x, height - 1, Direction.Up),
    for (y <- 0 until (height - 1)) yield Beam(0, y, Direction.Right),
    for (y <- 0 until (height - 1)) yield Beam(width - 1, y, Direction.Left),
  ).flatten


  val energized = allInitialBeams.map(beam => followBeams(input, Vector(beam), Set(beam))).max
  pprintln(energized)
}

part2()
