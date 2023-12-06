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


// def parseMap(mapData: List[List[String]]): Map[Long, Long] = {
//   val List(List(mapInfo), mapRanges) = mapData
//   val Array(srcCategory, _to, destCategory) = mapInfo.stripSuffix(" map:").split('-')
//   val ranges = mapRanges.map(_.split(' ').map(_.trim.toLong).toList)

//   val finMap: Map[Long, Long] = ranges.foldLeft(Map.empty)((map, range) => {
//     val List(dest, src, len) = range
//     val currentMap: Map[Long, Long] = (0L until len).foldLeft(Map.empty)((acc, i: Long) =>
//       acc.updated(src + i, dest + i))
//     map ++ currentMap
//   })

//   return finMap.withDefault(x => x)
// }

def parseMap(mapData: List[List[String]]): List[(Long, Long, Long)] = {
  val List(List(_mapInfo), mapRanges) = mapData
  val ranges = mapRanges.map(_.split(' ').map(_.trim.toLong).toList).map(x => (x(1), x(0), x(2)))


  return ranges
}


// def findLocationForSeed[A: Numeric](seed: A, maps: List[Map[A, A]]): A = {
//   maps.foldLeft(seed)((src, map) => map(src))
// }

def findLocationForSeed(seed: Long, maps: List[List[(Long, Long, Long)]]): Long = {
  maps.foldLeft(seed)((src, map) => {
    map.find(r => (r._1 to (r._1 + r._3)).contains(src)) match {
      case None => src
      case Some((s, d, l)) => src - s + d
    }
  })
}


def part1() = {
  val input = Source
    .fromFile("./day5-input.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList
    .partitionBy(_.endsWith("map:"))

  val seeds = input
    .head
    .head
    .stripPrefix("seeds: ")
    .split(' ')
    .map(_.trim.toLong)
    // .iterator
    // .grouped(2)
    // .flatMap(x => x(0) until (x(0) + x(1)))
    // .length
    // .toList


  // pprintln(seeds)
  // pprintln(seeds.distinct.size)
  // pprintln(seeds)

  val maps = input.tail.grouped(2).toList.map(parseMap)

  // pprintln(input)
  // pprintln(seeds)
  // pprintln(maps)
  // pprintln(parseMap(maps.head))

  val locations = seeds.map(findLocationForSeed(_, maps))

  // pprintln(locations)
  println(locations.min)
}

// part1()


def part2() = {
  val input = Source
    .fromFile("./day5-input.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList
    .partitionBy(_.endsWith("map:"))

  val seeds = input
    .head
    .head
    .stripPrefix("seeds: ")
    .split(' ')
    .map(_.trim.toLong)
    .iterator
    .grouped(2)
    .flatMap(x => x(0) until (x(0) + x(1)))
    // .length
    // .toList


  pprintln(seeds)
  // pprintln(seeds.distinct.size)
  // pprintln(seeds)

  val maps = input.tail.grouped(2).toList.map(parseMap)

  // pprintln(input)
  // pprintln(seeds)
  // pprintln(maps)
  // pprintln(parseMap(maps.head))

  val locations = seeds.map(findLocationForSeed(_, maps))

  // pprintln(locations)
  println(locations.min)
}

// part2()


def findSeedForLocation(location: Long, maps: List[List[(Long, Long, Long)]]): Long = {
  // maps.foldRight(location)((dest, map) => {
  //   map.find(r => (r._1 to (r._1 + r._3)).contains(src)) match {
  //     case None => src
  //     case Some((s, d, l)) => src - s + d
  //   }
  // })

  maps.foldRight(location)((map, dest) => {
    map.find(r => (r._2 to (r._2 + r._3)).contains(dest)) match {
      case None => dest
      case Some((s, d, l)) => dest - d + s
    }
  })
}


def part2Optimized() = {
  val input = Source
    .fromFile("./day5-input.txt")
    .getLines
    .filterNot(_.isBlank)
    .toList
    .partitionBy(_.endsWith("map:"))

  val seeds = input
    .head
    .head
    .stripPrefix("seeds: ")
    .split(' ')
    .map(_.trim.toLong)
    .iterator
    .grouped(2)
    .map(x => x(0) until (x(0) + x(1)))
    // .length
    .toList


  pprintln(seeds.size)
  // pprintln(seeds.distinct.size)
  // pprintln(seeds)

  val maps = input.tail.grouped(2).toList.map(parseMap)

  // pprintln(input)
  // pprintln(seeds)
  // pprintln(maps)
  // pprintln(parseMap(maps.head))

  // val locations = seeds.map(findLocationForSeed(_, maps))

  // pprintln(locations)
  // println(locations.min)
  val found = Iterator
    .from(0)
    .tapEach(x => if (x % 1_000_000 == 0) pprintln(x))
    .find(x => seeds.exists(_.contains(findSeedForLocation(x, maps))))
    .getOrElse(-1)

  pprintln(found)
}

part2Optimized()
