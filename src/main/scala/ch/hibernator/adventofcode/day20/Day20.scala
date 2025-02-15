package ch.hibernator.adventofcode.day20

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day20.Tile.Wall
import ch.hibernator.adventofcode.util.Direction4
import ch.hibernator.adventofcode.util.Direction4.{Down, Up}
import ch.hibernator.adventofcode.util.mutable.FullGridXy
import ch.hibernator.adventofcode.util.mutable.FullGridXy.Location

import scala.annotation.tailrec
import scala.collection.mutable

object Day20 extends SolutionBaseSimple:
  override def day: Int = 20

  override def solve(input: Seq[String]): (Long, Long) =
    given Show[Tile] = Show.fromToString[Tile]

    val sideLength = input.size

    val rawGrid = Array.fill(sideLength)(Array.fill(sideLength)(Tile.Wall))
    for
      y <- 0 until sideLength
      x <- 0 until sideLength
    do rawGrid(x)(y) = Tile.forKey(input(y)(x))

    val grid = FullGridXy[Tile](rawGrid)
//    println(grid.asText)

    val startLocation = grid.findValueLocation(Tile.Start)

    // Builds a map of track location to default time it takes to get to the location
    @tailrec
    def processTrack(
        location: Location,
        trackLocationToTime: mutable.Map[Location, Int],
        time: Int
    ): mutable.Map[Location, Int] =
      trackLocationToTime += ((location, time))
      if grid.get(location) == Tile.End then trackLocationToTime
      else
        val nextTrackLocations = grid
          .conditionalNeighbors(location)(tile => tile == Tile.Track || tile == Tile.End)
          .filterNot(trackLocationToTime.contains)
        assert(nextTrackLocations.sizeIs == 1)
        processTrack(nextTrackLocations.head, trackLocationToTime, time + 1)

    val trackLocationToTime = processTrack(startLocation, mutable.LinkedHashMap[Location, Int](), 0)
    val defaultTrackTime = trackLocationToTime.size - 1

    // Returns a cheat with start and end point to bypass the wall at the specified location (part 1 only)
    def cheatPointStartEnd(location: Location): Option[Cheat] =
      assert(grid.get(location) == Tile.Wall)
      if grid.getNeighborLocationValue(location, Up).exists(_.value.isOnTrack) && grid
          .getNeighborLocationValue(location, Down)
          .exists(_.value.isOnTrack)
      then
        val upTime = trackLocationToTime(location.move(Up))
        val downTime = trackLocationToTime(location.move(Down))
        if upTime > downTime then Some(Cheat(location.move(Down), location.move(Up)))
        else Some(Cheat(location.move(Up), location.move(Down)))
      else if grid.getNeighborLocationValue(location, Direction4.Left).exists(_.value.isOnTrack) && grid
          .getNeighborLocationValue(location, Direction4.Right)
          .exists(_.value.isOnTrack)
      then
        val leftTime = trackLocationToTime(location.move(Direction4.Left))
        val rightTime = trackLocationToTime(location.move(Direction4.Right))
        if leftTime > rightTime then Some(Cheat(location.move(Direction4.Right), location.move(Direction4.Left)))
        else Some(Cheat(location.move(Direction4.Left), location.move(Direction4.Right)))
      else None

    // Checks if the wall location can be bypassed by a cheat (part 1 only)
    def isCheatPoint(location: Location): Boolean =
      (grid
        .getNeighborLocationValue(location, Up)
        .exists(_.value.isOnTrack) && grid.getNeighborLocationValue(location, Down).exists(_.value.isOnTrack)) || (grid
        .getNeighborLocationValue(location, Direction4.Right)
        .exists(_.value.isOnTrack) && grid
        .getNeighborLocationValue(location, Direction4.Left)
        .exists(_.value.isOnTrack))

    // Calculates time saved by the cheat
    def cheatTimeSave(cheatPoint: Cheat): Int =
      trackLocationToTime(cheatPoint.end) - trackLocationToTime(cheatPoint.start) - cheatPoint.start
        .manhattanDistanceTo(cheatPoint.end)
        .toInt

    val result1 = grid
      .findLocationsWithValue(Tile.Wall)
      .filter(isCheatPoint)
      .flatMap(cheatPointStartEnd)
      .map(cheatTimeSave)
      .count(_ >= 100)

    // Part 2
    val result2 = trackLocationToTime.map { (location, defaultTime) =>
      grid
        .locationsWithinManhattanDistanceFrom(location, 20, 2)
        .filterNot(grid.get(_) == Wall)
        .map(Cheat(location, _))
        .map(cheatTimeSave)
        .count(_ >= 100)
    }.sum

    (result1, result2)

enum Tile(val key: Char):
  case Track extends Tile('.')
  case Wall extends Tile('#')
  case Start extends Tile('S')
  case End extends Tile('E')

  def isOnTrack: Boolean = this != Wall

  override def toString: String = key.toString

object Tile:
  def forKey(keyChar: Char): Tile = Tile.values.find(_.key == keyChar).get

case class Cheat(start: Location, end: Location)
