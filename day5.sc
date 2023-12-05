import scala.io.Source
import pprint.pprintln


implicit class PartitionByOnList[A](xs: List[A]) {
  def partitionBy[B](f: A => B): List[List[A]] = {
    if (xs.isEmpty) {
      List()
    } else {
      var allLists: List[List[A]] = List()
      var currentValue = f(xs.head)
      var currentList: List[A] = List(xs.head)
      for (element <- xs.tail) {
        val elementValue = f(element)
        if (elementValue == currentValue) {
          currentList = currentList :+ element
        } else {
          allLists = allLists :+ currentList
          currentValue = elementValue
          currentList = List(element)
        }
      }
      allLists :+ currentList
    }
  }
}


def parseMap(mapData: List[List[String]]): (String, String, Map[Int, Int]) = {
  val List(List(mapInfo), mapRanges) = mapData
  val Array(srcCategory, _to, destCategory) = mapInfo.stripSuffix(" map:").split('-')
  val ranges = mapRanges.map(_.split(' ').map(_.trim.toInt).toList)
  pprintln(ranges)
  // var map: Map[Int, Int] = Map().withDefault(x => x)

  val map: Map[Int, Int] = Map().withDefault(x => x)

  val finMap = ranges.foldLeft(Map[Int, Int]())((map, range) => {
    val List(dest, src, len) = range

    val r = (0 until len)
    val mi: Map[Int, Int] = r.foldLeft(Map.empty)((acc, i) => acc.updated(src + i, dest + i))

    pprintln(mi)

    map ++ mi
    // for (i <- 0 until len) {
    //   map = map.updated(src + i, dest + i)
    // }
  }).withDefault(x => x)

  pprintln(finMap(98))

  // ranges.foreach(range => {
  //   val List(dest, src, len) = range
  //   for (i <- 0 until len) {
  //     map = map.updated(src + i, dest + i)
  //   }
  // })

  return (srcCategory, destCategory, map)
}


def part1() = {
  val input = Source
    .fromFile("./day5-input-example.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList
    .partitionBy(_.endsWith("map:"))

  val seeds = input.head.head.stripPrefix("seeds: ").split(' ').map(_.trim.toInt)

  val maps = input.tail.grouped(2).toList

  pprintln(input)
  pprintln(seeds)
  pprintln(maps)
  pprintln(parseMap(maps.head))
}

part1()
