import scala.io.Source
import pprint.pprintln
import scala.util.matching.Regex
import scala.annotation.tailrec



type NodeName = String
type NodePath = (NodeName, NodeName)


def parseNode(line: String): (NodeName, NodePath)  = {
  val linePattern = """([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)""".r
  val result = linePattern.findFirstMatchIn(line).get
  (result.group(1), (result.group(2), result.group(3)))
}

@tailrec
def nextStep(nodes: Map[NodeName, NodePath], node: NodeName, directions: LazyList[Char], acc: Int): Int = {
  if (node == "ZZZ") {
    acc
  } else {
    val (left, right) = nodes.get(node).get
    val nextNode = directions.head match {
      case 'L' => left
      case 'R' => right
    }
    nextStep(nodes, nextNode, directions.tail, 1 + acc)
  }
}

def part1() = {
  val input = Source
    .fromFile("./input/day8.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val directions = LazyList.continually(input(0)).flatten

  // pprintln(directionsCycle.take(10))

  val nodes = input
    .tail
    .map(parseNode)
    .toMap

  val steps = nextStep(nodes, "AAA", directions, 0)


  // pprintln(input)
  // pprintln(nodes)
  pprintln(steps)
}

part1()



@tailrec
def nextSteps(
  nodes: Map[NodeName, NodePath],
  currentNodes: List[NodeName],
  directions: LazyList[Char],
  acc: Int
): Int = {
  if (currentNodes.forall(_.endsWith("Z"))) {
    acc
  } else {
    val nextNodes = currentNodes.map(node => {
      val (left, right) = nodes.get(node).get
      directions.head match {
        case 'L' => left
        case 'R' => right
      }
    })
    nextSteps(nodes, nextNodes, directions.tail, 1 + acc)
  }
}


def toCamelCase(str: String): String = {
  val Array(firstWord, otherWords*) = str.split("[-_]")
  // val firstWord = splits(0)
  val newFirstWord = firstWord.patch(1, firstWord.tail.toLowerCase, firstWord.length - 1)
  otherWords.map(_.capitalize).prepended(newFirstWord).mkString
}



"Hello".patch(1, "Hello".tail.toLowerCase, "Hello".length - 1)


def part2() = {
  val input = Source
    .fromFile("./examples/day8-2.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList

  val directions = LazyList.continually(input(0)).flatten

  // pprintln(directionsCycle.take(10))

  val nodes = input
    .tail
    .map(parseNode)
    .toMap

  val startingNodes = nodes.keys.filter(_.endsWith("A")).toList

  val steps = nextSteps(nodes, startingNodes, directions, 0)


  // pprintln(input)
  // pprintln(nodes)
  pprintln(steps)
}

part2()
