import scala.io.Source
import pprint.pprintln


def readInput(path: String): String =
  Source
    .fromFile(path)
    .getLines
    .filterNot(_.isBlank)
    .next


def hashPart(s: String): Long =
  s.foldLeft(0L)((acc, char) => (char.toInt + acc) * 17 % 256)


def part1() = {
  val input = readInput("./input/day15.txt")

  val result = input
    .split(',')
    .map(hashPart)
    .sum

  pprintln(result)
}

part1()


type Label = String
case class Lens(label: Label, focalLength: Int)
type Box = Vector[Lens]


def runStep(boxes: Vector[Box], step: String): Vector[Box] = {
  val label = """[a-z]+""".r.findFirstIn(step).get
  val hash = hashPart(label).toInt
  val operation = """[-=]""".r.findFirstIn(step).get
  val newBoxAtHash = operation match {
    case "-" => boxes(hash).filter(_.label != label)
    case "=" => {
      val focalLength = step.last.asDigit
      boxes(hash).indexWhere(_.label == label) match {
        case -1 => boxes(hash).appended(Lens(label, focalLength))
        case index => boxes(hash).updated(index, Lens(label, focalLength))
      }
    }
  }
  boxes.updated(hash, newBoxAtHash)
}

def computeFocusingPower(boxes: Vector[Box]): Long = {
  boxes
    .zipWithIndex
    .map((box, boxIdx) =>
      box
        .zipWithIndex
        .map((lens, lensIdx) => (boxIdx + 1) * (lensIdx + 1) * lens.focalLength)
        .sum)
    .sum
}


def part2() = {
  val input = readInput("./input/day15.txt")

  val startingBoxes: Vector[Box] = Vector.fill(256)(Vector.empty[Lens])

  val finalBoxes = input.split(',').foldLeft(startingBoxes)(runStep)

  pprintln(computeFocusingPower(finalBoxes))
}

part2()
