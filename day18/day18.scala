import scala.collection.mutable.{HashSet, Queue}
import scala.io.Source

def neighbors(x: Int, y: Int, z: Int): List[(Int, Int, Int)] = List(
  (x + 1, y, z),
  (x - 1, y, z),
  (x, y + 1, z),
  (x, y - 1, z),
  (x, y, z + 1),
  (x, y, z - 1)
)

def freeNeighbors(
    pos: (Int, Int, Int),
    droplet: HashSet[(Int, Int, Int)],
    mins: Array[Int],
    maxs: Array[Int]
): List[(Int, Int, Int)] =
  val freeNeighbors = neighbors.tupled(pos).filter { case (x, y, z) =>
    !droplet.contains((x, y, z)) &&
    x >= mins(0) && x <= maxs(0) &&
    y >= mins(1) && y <= maxs(1) &&
    z >= mins(2) && z <= maxs(2)
  }
  return freeNeighbors

def surfaceArea(droplet: List[(Int, Int, Int)]): Int =
  var seen: HashSet[(Int, Int, Int)] = HashSet()
  var area: Int = 0
  for coords <- droplet do
    area += 6
    seen += coords
    for neighbor <- neighbors.tupled(coords) do
      if seen.contains(neighbor) then area -= 2
  return area

def findInterior(coords: List[(Int, Int, Int)]): List[(Int, Int, Int)] =
  val droplet = HashSet() ++ coords
  var visited: HashSet[(Int, Int, Int)] = HashSet()
  val mins = Array(-1, -1, -1)
  val maxs =
    Array(coords.map(_._1).max, coords.map(_._2).max, coords.map(_._3).max)
  var queue = Queue((-1, -1, -1))
  while queue.length > 0 do
    val v = queue.dequeue()
    for neighbor <- freeNeighbors(v, droplet, mins, maxs).filter(
        !visited.contains(_)
      )
    do
      queue += neighbor
      visited += neighbor
  val interior = (for {
    x <- 0 until maxs(0)
    y <- 0 until maxs(1)
    z <- 0 until maxs(2)
    if !visited.contains((x, y, z)) && !droplet.contains((x, y, z))
  } yield (x, y, z)).toList
  return interior

@main def main: Unit =
  val source = Source.fromFile("input.txt")
  val raw_data =
    try source.mkString
    finally source.close()
  val coords = raw_data
    .split("\n")
    .map(_.split(","))
    .map(_.map(_.toInt))
    .map { case Array(x, y, z) => (x, y, z) }
    .toList
  val totalArea = surfaceArea(coords)
  println(f"Part 1: $totalArea")
  val interiorArea = surfaceArea(findInterior(coords))
  println(f"Part 2: ${totalArea - interiorArea}")
