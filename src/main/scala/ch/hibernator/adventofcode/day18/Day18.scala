package ch.hibernator.adventofcode.day18

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.mutable.FullGridXy
import ch.hibernator.adventofcode.util.mutable.FullGridXy.Location

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day18 extends SolutionBaseSimple:
  override def day: Int = 18

  override def solve(input: Seq[String]): (Long, Long) =
    val fallingBytes = input.map(_.split(",").map(_.toInt).pipe(location => Location(location.head, location.last)))
    val isTest = fallingBytes.size < 30
    val gridSize = if isTest then 7 else 71
    val grid = FullGridXy[Tile](Array.fill(gridSize)(Array.fill(gridSize)(Tile.Empty)))
    val bytesToFall = if isTest then 12 else 1024
    fallingBytes.take(bytesToFall).foreach(fallenByte => grid.set(fallenByte, Tile.Byte))
    given Show[Tile] = Show.fromToString

    println(grid.asText)

    // Dijkstra algorithm with the following optimizations
    // - only new nodes are put into frontier - priority queue
    // - we stop once the destination is reached
    def findShortestPath: Seq[Location] =
      val start = Location(0, 0)
      val end = Location(gridSize - 1, gridSize - 1)
      val frontier = mutable.PriorityQueue(LocationWithDistanceFromStart(start, 0))
      val locationByDistanceFromStart = mutable.Map[Location, Int]().withDefaultValue(Int.MaxValue)
      val locationByPrevious = mutable.Map[Location, Location]()

      while frontier.nonEmpty do
        val locationToProcess = frontier.dequeue()
        val neighbors =
          grid.neighboringLocations(locationToProcess.location).filterNot(location => grid.get(location) == Tile.Byte)
        neighbors.foreach { neighbor =>
          val alternativeDistance = locationToProcess.distance + 1
          if alternativeDistance < locationByDistanceFromStart(neighbor) then
            locationByPrevious.addOne(neighbor, locationToProcess.location)
            locationByDistanceFromStart.addOne(neighbor, alternativeDistance)
            frontier.addOne(LocationWithDistanceFromStart(neighbor, alternativeDistance))
        }

      @tailrec
      def constructPathFromEnd(pathAcc: mutable.Buffer[Location], next: Location): mutable.Buffer[Location] =
        pathAcc.append(next)
        if next == start then pathAcc
        else constructPathFromEnd(pathAcc, locationByPrevious(next))

      if locationByPrevious.contains(end) then constructPathFromEnd(mutable.Buffer[Location](), end).toSeq.reverse
      else Seq()

    val shortestPath = findShortestPath
    shortestPath.foreach(location => grid.set(location, Tile.Path))

    println(grid.asText)

    @tailrec
    def findFirstBlockingByte(remainingBytes: Seq[Location]): Location =
      val nextByte = remainingBytes.head
      grid.set(nextByte, Tile.Byte)
      val path = findShortestPath
      if path.isEmpty then nextByte else findFirstBlockingByte(remainingBytes.tail)

    val blockingByte = findFirstBlockingByte(fallingBytes.drop(bytesToFall))
    println(blockingByte)

    (shortestPath.size - 1, 1L)

enum Tile(representation: Char):
  override def toString: String = representation.toString

  case Empty extends Tile('.')
  case Byte extends Tile('#')
  case Path extends Tile('O')

object Tile:
  def fromChar(char: Char): Tile =
    char match
      case '.' => Empty
      case '#' => Byte

case class LocationWithDistanceFromStart(location: Location, distance: Int)

object LocationWithDistanceFromStart:
  // Items with the lowest distance have the highest priority
  given Ordering[LocationWithDistanceFromStart] = Ordering.fromLessThan((loc1, loc2) => loc1.distance > loc2.distance)
