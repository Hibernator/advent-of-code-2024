package ch.hibernator.adventofcode.day4

import ch.hibernator.adventofcode.SolutionBase
import ch.hibernator.adventofcode.day4.Direction.*

import scala.collection.mutable

object Day4 extends SolutionBase[Puzzle]:
  private val xmas = Seq('X', 'M', 'A', 'S')

  override def day: Int = 4

  override def intermediateResult(input: Seq[String]): Puzzle =
    val puzzle = input.map(_.toCharArray).toArray
    assert(puzzle.forall(_.length == puzzle.head.length))
    Puzzle(puzzle)

  override def solvePart1(input: Seq[String], puzzle: Puzzle): Long =
    val positionsOfX = puzzle.findPositionsOfChar('X')
    var counter = 0L
    positionsOfX.foreach { position =>
      Direction.values.foreach { direction =>
        if puzzle.eligibleXmasWord(direction, position) && puzzle.getCharsInDirection(position, direction) == xmas
        then counter = counter + 1
      }
    }
    counter

  override def solvePart2(input: Seq[String], puzzle: Puzzle): Long =
    val positionsOfA = puzzle.findPositionsOfChar('A')
    var counter = 0
    positionsOfA.foreach { position =>
      if puzzle.eligibleXmasSign(position) && puzzle.isXmasSign(position) then counter = counter + 1
    }
    counter

end Day4

class Puzzle(data: Array[Array[Char]]):

  private def getChar(coordinates: Coordinates): Char =
    data(coordinates.row)(coordinates.column)

  private def getChar(row: Int, col: Int): Char =
    data(row)(col)

  val numRows: Int = data.length
  val numCols: Int = data.head.length

  def findPositionsOfChar(char: Char): Set[Coordinates] =
    val acc: mutable.Set[Coordinates] = mutable.Set()
    for
      row <- 0 until numRows
      col <- 0 until numCols
    do if getChar(row, col) == char then acc.addOne(Coordinates(row, col))
    acc.toSet

  def eligibleXmasWord(direction: Direction, coordinates: Coordinates): Boolean =
    direction match
      case North     => coordinates.row > 2
      case NorthEast => eligibleXmasWord(North, coordinates) && eligibleXmasWord(East, coordinates)
      case East      => coordinates.column < numCols - 3
      case SouthEast => eligibleXmasWord(South, coordinates) && eligibleXmasWord(East, coordinates)
      case South     => coordinates.row < numRows - 3
      case SouthWest => eligibleXmasWord(South, coordinates) && eligibleXmasWord(West, coordinates)
      case West      => coordinates.column > 2
      case NorthWest => eligibleXmasWord(North, coordinates) && eligibleXmasWord(West, coordinates)

  def eligibleXmasSign(coordinates: Coordinates): Boolean =
    coordinates.row > 0 && coordinates.column > 0 && coordinates.row < numRows - 1 && coordinates.column < numCols - 1

  def isXmasSign(coordinates: Coordinates): Boolean =
    (getChar(coordinates.move(NorthWest, 1)) == 'M' && getChar(coordinates.move(SouthEast, 1)) == 'S' || getChar(
      coordinates.move(NorthWest, 1)
    ) == 'S' && getChar(coordinates.move(SouthEast, 1)) == 'M') &&
      (getChar(coordinates.move(NorthEast, 1)) == 'M' && getChar(coordinates.move(SouthWest, 1)) == 'S' || getChar(
        coordinates.move(NorthEast, 1)
      ) == 'S' && getChar(coordinates.move(SouthWest, 1)) == 'M')

  def getCharsInDirection(coordinates: Coordinates, direction: Direction): Seq[Char] =
    direction match
      case North =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(North, 1)),
          getChar(coordinates.move(North, 2)),
          getChar(coordinates.move(North, 3))
        )
      case NorthEast =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(NorthEast, 1)),
          getChar(coordinates.move(NorthEast, 2)),
          getChar(coordinates.move(NorthEast, 3))
        )
      case East =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(East, 1)),
          getChar(coordinates.move(East, 2)),
          getChar(coordinates.move(East, 3))
        )
      case SouthEast =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(SouthEast, 1)),
          getChar(coordinates.move(SouthEast, 2)),
          getChar(coordinates.move(SouthEast, 3))
        )
      case South =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(South, 1)),
          getChar(coordinates.move(South, 2)),
          getChar(coordinates.move(South, 3))
        )
      case SouthWest =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(SouthWest, 1)),
          getChar(coordinates.move(SouthWest, 2)),
          getChar(coordinates.move(SouthWest, 3))
        )
      case West =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(West, 1)),
          getChar(coordinates.move(West, 2)),
          getChar(coordinates.move(West, 3))
        )
      case NorthWest =>
        Seq(
          getChar(coordinates),
          getChar(coordinates.move(NorthWest, 1)),
          getChar(coordinates.move(NorthWest, 2)),
          getChar(coordinates.move(NorthWest, 3))
        )

enum Direction:
  case North
  case NorthEast
  case East
  case SouthEast
  case South
  case SouthWest
  case West
  case NorthWest

case class Coordinates(row: Int, column: Int):
  def move(direction: Direction, distance: Int): Coordinates =
    direction match
      case North     => copy(row = row - distance)
      case NorthEast => copy(row = row - distance, column + distance)
      case East      => copy(column = column + distance)
      case SouthEast => copy(row = row + distance, column = column + distance)
      case South     => copy(row = row + distance)
      case SouthWest => copy(row = row + distance, column = column - distance)
      case West      => copy(column = column - distance)
      case NorthWest => copy(row = row - distance, column = column - distance)
