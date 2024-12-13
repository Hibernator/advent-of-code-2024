package ch.hibernator.adventofcode.util

import ch.hibernator.adventofcode.util.Direction4.{Down, Up}

case class Coordinates(row: Int, column: Int):
  def move(direction: Direction4): Coordinates =
    direction match
      case Up               => Coordinates(row - 1, column)
      case Down             => Coordinates(row + 1, column)
      case Direction4.Left  => Coordinates(row, column - 1)
      case Direction4.Right => Coordinates(row, column + 1)

  def allNeighbors4: Seq[Coordinates] =
    Direction4.values.map(direction => move(direction))

  override def toString: String = s"[$row,$column]"
