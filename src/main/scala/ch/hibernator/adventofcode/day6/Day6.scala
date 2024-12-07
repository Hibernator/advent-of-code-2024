package ch.hibernator.adventofcode.day6

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day6.Direction.{Down, Left, Right, Up}

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

object Day6 extends SolutionBaseSimple:
  override def day: Int = 6

  override def solve(input: Seq[String]): (Long, Long) =
    // Common stuff
    val numRows = input.size
    val numColumns = input.head.length
    val area = input.map(_.toCharArray).toArray
    val obstacles = mutable.Set[Position]()
    var guardStartingPosition: GuardPosition = null

    for
      row <- 0 until numRows
      column <- 0 until numColumns
    do
      val point = area(row)(column)
      if point == '#' then obstacles += Position(row, column)
      if point == '^' then guardStartingPosition = GuardPosition(Position(row, column), Up)

    val immutableObstacles = obstacles.toSet
    val fixedGuardStartingPosition = guardStartingPosition.copy()

    var guardPosition = fixedGuardStartingPosition.copy()

    def isInsideArea(guardPos: GuardPosition) =
      val position = guardPos.position
      position.row > -1 && position.row < numRows && position.column > -1 && position.column < numColumns

    def moveGuard(currentObstacles: Set[Position], guardPos: GuardPosition): GuardPosition =
      if currentObstacles.contains(guardPos.nextPosition)
      then
        val newGuardPos = guardPos.move(guardPos.direction.next)
        if currentObstacles.contains(newGuardPos.position)
        then guardPos.move(guardPos.direction.next.next) // in case a corner is hit
        else guardPos.move(guardPos.direction.next)
      else guardPos.move(guardPos.direction)

    // part 1 solution
    val visitedLocations = mutable.Set[Position]()
    while isInsideArea(guardPosition)
    do
      visitedLocations += guardPosition.position
      guardPosition = moveGuard(immutableObstacles, guardPosition)

    // part 2 solution
    val possibleObstacleLocations = mutable.Set[Position]()
    for
      row <- 0 until numRows
      column <- 0 until numColumns
    do
      val point = area(row)(column)
      if point == '.' then possibleObstacleLocations += Position(row, column)

    def isNewObstacleFeasible(newObstaclePosition: Position) =
      var result = true
      val newObstacles = immutableObstacles + newObstaclePosition
      val visitedLocationsWithDirection = mutable.Set[GuardPosition]()
      var guardPos = fixedGuardStartingPosition.copy()

      breakable {
        while true
        do
          if !isInsideArea(guardPos) then
            result = false
            break()
          if visitedLocationsWithDirection.contains(guardPos)
          then
            result = true
            break()
          visitedLocationsWithDirection += guardPos
          guardPos = moveGuard(newObstacles, guardPos)
      }
      result

    val result2 = possibleObstacleLocations.count(isNewObstacleFeasible)

    (visitedLocations.size, result2)

enum Direction:
  case Up, Down, Left, Right

  def next: Direction = this match
    case Up    => Right
    case Right => Down
    case Down  => Left
    case Left  => Up

case class Position(row: Int, column: Int):
  def move(direction: Direction): Position =
    direction match
      case Up    => copy(row = row - 1)
      case Down  => copy(row = row + 1)
      case Left  => copy(column = column - 1)
      case Right => copy(column = column + 1)

case class GuardPosition(position: Position, direction: Direction):
  def nextPosition: Position = position.move(direction)
  def move(direction: Direction): GuardPosition = GuardPosition(position.move(direction), direction)
