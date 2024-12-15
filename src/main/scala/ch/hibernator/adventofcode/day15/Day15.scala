package ch.hibernator.adventofcode.day15

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day15.Value.*
import ch.hibernator.adventofcode.util.Direction4
import ch.hibernator.adventofcode.util.Direction4.{Down, Up}
import ch.hibernator.adventofcode.util.InputOps.numRowsAndColumns
import ch.hibernator.adventofcode.util.mutable.FullGridXy
import ch.hibernator.adventofcode.util.mutable.FullGridXy.Location

import scala.collection.mutable

object Day15 extends SolutionBaseSimple:
  override def day: Int = 15

  override def solve(input: Seq[String]): (Long, Long) =
    // Initialization
    val rawGrid = input.takeWhile(_.nonEmpty)
    val rawMoves = input.drop(rawGrid.size + 1)
    val (numRows, numColumns) = rawGrid.numRowsAndColumns
    val gridArray = Array.fill(numColumns)(Array.fill(numRows)(Empty))
    for
      y <- 0 until numRows
      x <- 0 until numColumns
    do gridArray(x)(y) = Value.fromChar(rawGrid(y)(x))
    var grid = FullGridXy[Value](gridArray)
    val moves = mutable.Buffer[Direction4]()
    rawMoves.foreach { row =>
      row.foreach { rawMove =>
        val move = rawMove match
          case '<' => Direction4.Left
          case '^' => Direction4.Up
          case 'v' => Direction4.Down
          case '>' => Direction4.Right
        moves.append(move)
      }
    }

    // First part
    var robotLocation = grid.findValueLocation(Value.Robot)

    def moveAnything(location: Location, direction: Direction4): Boolean =
      def moveWideBox(leftPartLocation: Location, rightPartLocation: Location): Boolean =
        assert(direction == Up || direction == Down)
        val currentGridState = grid.copy
        val leftPartNextLocation = leftPartLocation.move(direction)
        val rightPartNextLocation = rightPartLocation.move(direction)
        val leftPartNeighborMoved = moveAnything(leftPartNextLocation, direction)
        val rightPartNeighborMoved = moveAnything(rightPartNextLocation, direction)
        if leftPartNeighborMoved && rightPartNeighborMoved then
          grid.set(leftPartNextLocation, BoxLeft)
          grid.set(leftPartLocation, Empty)
          grid.set(rightPartNextLocation, BoxRight)
          grid.set(rightPartLocation, Empty)
          true
        else
          grid = currentGridState
          false

      val valueAtLocation = grid.get(location)
      valueAtLocation match
        case Empty => true
        case Wall  => false
        case boxOrRobot @ (Box | Robot) =>
          val nextLocation = location.move(direction)
          // stack recursion, could run into trouble
          val neighborMoved = moveAnything(nextLocation, direction)
          if neighborMoved then
            grid.set(nextLocation, boxOrRobot)
            grid.set(location, Empty)
            if boxOrRobot == Robot then robotLocation = nextLocation
            true
          else false
        case BoxLeft =>
          direction match
            case Direction4.Left | Direction4.Right =>
              val nextLocation = location.move(direction)
              val neighborMoved = moveAnything(nextLocation, direction)
              if neighborMoved then
                grid.set(nextLocation, BoxLeft)
                grid.set(location, Empty)
                true
              else false
            case Up | Down =>
              val boxRightLocation = location.move(Direction4.Right)
              val boxMoved = moveWideBox(location, boxRightLocation)
              boxMoved
        case BoxRight =>
          direction match
            case Direction4.Left | Direction4.Right =>
              val nextLocation = location.move(direction)
              val neighborMoved = moveAnything(nextLocation, direction)
              if neighborMoved then
                grid.set(nextLocation, BoxRight)
                grid.set(location, Empty)
                true
              else false
            case Up | Down =>
              val boxLeftLocation = location.move(Direction4.Left)
              val boxMoved = moveWideBox(boxLeftLocation, location)
              boxMoved

    moves.foreach(moveAnything(robotLocation, _))

    def gpsSum: Long =
      var sum = 0L
      for
        x <- 0 to grid.maxX
        y <- 0 to grid.maxY
      do
        val value = grid.get(x, y)
        if value == Box || value == BoxLeft
        then sum += x + 100 * y
      sum

    val gpsSum1 = gpsSum

    // Second part
    val (numRows2, numColumns2) = (numRows, numColumns * 2)
    val gridArray2 = Array.fill(numColumns2)(Array.fill(numRows2)(Empty))
    grid = FullGridXy[Value](gridArray2)

    for
      y <- 0 until numRows
      x <- 0 until numColumns
    do
      val value = Value.fromChar(rawGrid(y)(x))
      val (x1, y1, x2, y2) = (x * 2, y, x * 2 + 1, y)
      value match
        case Empty =>
          grid.set(x1, y1, Empty)
          grid.set(x2, y2, Empty)
        case Wall =>
          grid.set(x1, y1, Wall)
          grid.set(x2, y2, Wall)
        case Box =>
          grid.set(x1, y1, BoxLeft)
          grid.set(x2, y2, BoxRight)
        case Robot =>
          grid.set(x1, y1, Robot)
          grid.set(x2, y2, Empty)

    robotLocation = grid.findValueLocation(Value.Robot)

    moves.foreach(moveAnything(robotLocation, _))
    val gpsSum2 = gpsSum

    (gpsSum1, gpsSum2)

  given Show[Value] = Show.fromToString

enum Value(representation: Char):
  override def toString: String = representation.toString

  case Empty extends Value('.')
  case Box extends Value('O')
  case Robot extends Value('@')
  case Wall extends Value('#')
  case BoxLeft extends Value('[')
  case BoxRight extends Value(']')

object Value:
  def fromChar(char: Char): Value =
    char match
      case '.' => Empty
      case 'O' => Box
      case '@' => Robot
      case '#' => Wall
