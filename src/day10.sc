import scala.io.Source
import pprint.pprintln
import scala.annotation.tailrec


enum Direction:
  case North, East, South, West

case class Move(x: Int, y: Int, direction: Direction)

def pipeDirections: Map[Char, List[Direction]] = Map(
  '|' -> List(Direction.North, Direction.South),
  '-' -> List(Direction.West, Direction.East),
  'L' -> List(Direction.North, Direction.East),
  'J' -> List(Direction.North, Direction.West),
  '7' -> List(Direction.South, Direction.West),
  'F' -> List(Direction.South, Direction.East),
)


def invertDirection(d: Direction): Direction = {
  d match {
    case Direction.East  => Direction.West
    case Direction.West  => Direction.East
    case Direction.North => Direction.South
    case Direction.South => Direction.North
  }
}

def findStartMove(grid: List[List[Char]]): Move  = {
  val startCoords: IndexedSeq[(Int, Int)] = for {
    y <- 0 until grid.length
    x <- 0 until grid(0).length
    if grid(y)(x) == 'S'
  } yield (x, y)
  val start = startCoords(0)

  // pprintln("start")
  // pprintln(start)


  val startMoves: List[Move] = List(
    Move(x = start._1 - 1, y = start._2 + 0, Direction.East),
    Move(x = start._1 + 1, y = start._2 + 0, Direction.West),
    Move(x = start._1 + 0, y = start._2 - 1, Direction.South),
    Move(x = start._1 + 0, y = start._2 + 1, Direction.North),
  )

  val possibleMoves = startMoves
    .flatMap(move => {
      val nextTile = grid(move.y)(move.x)
      nextTile match {
        case '.' => None
        case tile if pipeDirections(tile).contains(move.direction) => Some(move)
        case _ => None
      }
    })

  // pprintln(possibleMoves)

  possibleMoves(0)
}


@tailrec
def followLoop(grid: List[List[Char]], move: Move, movecount: Int): Int = {
  val nextTile = grid(move.y)(move.x)
  if (nextTile == 'S') {
    movecount
  } else {
    val nextMove: Move = nextTile match {
      case '-' => Move(
        y = move.y,
        x = move.direction match {
          case Direction.East => move.x - 1
          case Direction.West => move.x + 1
          case _ => move.x
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
      case '|' => Move(
        x = move.x,
        y = move.direction match {
          case Direction.North => move.y + 1
          case Direction.South => move.y - 1
          case _ => move.y
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
      case 'L' => Move(
        x = move.direction match {
          case Direction.North => move.x + 1
          case _ => move.x
        },
        y = move.direction match {
          case Direction.East => move.y - 1
          case _ => move.y
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
      case 'J' => Move(
        x = move.direction match {
          case Direction.North => move.x - 1
          case _ => move.x
        },
        y = move.direction match {
          case Direction.West => move.y - 1
          case _ => move.y
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
      case '7' => Move(
        x = move.direction match {
          case Direction.South => move.x - 1
          case _ => move.x
        },
        y = move.direction match {
          case Direction.West => move.y + 1
          case _ => move.y
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
      case 'F' => Move(
        x = move.direction match {
          case Direction.South => move.x + 1
          case _ => move.x
        },
        y = move.direction match {
          case Direction.East => move.y + 1
          case _ => move.y
        },
        direction = invertDirection(pipeDirections(nextTile).filter(_ != move.direction).head)
      )
    }
    // pprintln(nextMove)

    followLoop(grid, nextMove, movecount + 1)
  }
}


def part1() = {
  val input = Source
    .fromFile("./input/day10.txt")
    .getLines
    .filterNot(_.isBlank)
    .map(_.toList)
    .toList
  // pprintln(input)

  val width = input(0).length
  val paddedInput = input
    .map('.' +: _ :+ '.')
    .prepended(("." * (width + 2)).toList)
    .appended(("." * (width + 2)).toList)

  // pprintln(paddedInput)

  val startMove = findStartMove(paddedInput)

  // pprintln(startMove)


  val steps = followLoop(paddedInput, startMove, 1)
  pprintln(steps)
  pprintln(steps / 2)
}

part1()
