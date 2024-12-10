package ch.hibernator.adventofcode.day10

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.collection.mutable

object Day10 extends SolutionBaseSimple:
  override def day: Int = 10

  override def solve(input: Seq[String]): (Long, Long) =
    val numRows = input.size
    val numColumns = input.head.length
    val gridMap: mutable.Map[Coordinates, Int] = mutable.LinkedHashMap()
    for
      rowIndex <- 0 until numRows
      columnIndex <- 0 until numColumns
    do gridMap.update(Coordinates(rowIndex, columnIndex), input(rowIndex)(columnIndex).asDigit)

    val grid = Grid(gridMap, numRows, numColumns)

    // BFS algorithm to find all peaks reachable from the trailhead
    def trailheadScore(trailhead: Coordinates): Int =
      val pointsToVisit: mutable.Queue[Coordinates] = mutable.Queue(trailhead)
      val visited: mutable.Set[Coordinates] = mutable.LinkedHashSet(trailhead)
      var currentScore = 0

      while pointsToVisit.nonEmpty
      do
        val currentPoint = pointsToVisit.dequeue()
        if grid.getHeight(currentPoint) == 9 then currentScore += 1
        val possibleMoves = grid.possibleMoves(currentPoint).filterNot(visited.contains)
        visited.addAll(possibleMoves)
        pointsToVisit.enqueueAll(possibleMoves)

      currentScore

    val allTrailheads = grid.filter(_._2 == 0)
    val result1 = allTrailheads.map(trailheadScore).sum

    // BFS again but we don't keep track of visited locations anymore, therefore a peak can be reached multiple times
    // The graph has no cycles, so it's safe
    def trailheadRating(trailhead: Coordinates): Int =
      val pointsToVisit: mutable.Queue[Coordinates] = mutable.Queue(trailhead)
      var currentScore = 0

      while pointsToVisit.nonEmpty
      do
        val currentPoint = pointsToVisit.dequeue()
        if grid.getHeight(currentPoint) == 9 then currentScore += 1
        val possibleMoves = grid.possibleMoves(currentPoint)
        pointsToVisit.enqueueAll(possibleMoves)

      currentScore

    val result2 = allTrailheads.map(trailheadRating).sum

    (result1, result2)

case class Coordinates(row: Int, column: Int):
  def move(direction: Direction): Coordinates =
    direction match
      case Direction.Up    => copy(row = row + 1)
      case Direction.Down  => copy(row = row - 1)
      case Direction.Left  => copy(column = column - 1)
      case Direction.Right => copy(column = column + 1)

enum Direction:
  case Up, Down, Left, Right

case class Grid(points: mutable.Map[Coordinates, Int], numRows: Int, numColumns: Int):
  def canMove(start: Coordinates, direction: Direction): Boolean =
    val startHeight = points(start)
    val nextPoint = start.move(direction)
    points.get(nextPoint).contains(startHeight + 1)

  def possibleMoves(start: Coordinates): Seq[Coordinates] =
    Direction.values.filter(canMove(start, _)).map(start.move)

  def getHeight(point: Coordinates): Int = points(point)

  def filter(predicate: ((Coordinates, Int)) => Boolean): Seq[Coordinates] = points.filter(predicate).keys.toSeq
