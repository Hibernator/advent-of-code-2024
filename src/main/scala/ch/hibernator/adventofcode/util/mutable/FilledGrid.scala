package ch.hibernator.adventofcode.util.mutable

import ch.hibernator.adventofcode.util.{Coordinates, CoordinatesWithValue}

class FilledGrid[T](grid: Array[Array[T]]):
  val numRows: Int = grid.length
  val numColumns: Int = grid.head.length

  def get(row: Int, column: Int): T = grid(row)(column)
  def get(coordinates: Coordinates): T = grid(coordinates.row)(coordinates.column)
  def neighboringCoordinates(coordinates: Coordinates): Seq[Coordinates] =
    coordinates.allNeighbors4.filter(isWithinBorders)
  def neighboringCoordinatesWithValues(coordinates: Coordinates): Seq[CoordinatesWithValue[T]] =
    neighboringCoordinates(coordinates).map(coordinates => CoordinatesWithValue(coordinates, get(coordinates)))

  def isWithinBorders(coordinates: Coordinates): Boolean = isWithinBorders(coordinates.row, coordinates.column)

  def isWithinBorders(row: Int, column: Int): Boolean = row > -1 && column > -1 && row < numRows && column < numColumns

  def conditionalNeighbors(coordinates: Coordinates)(
      filter: T => Boolean
  ): Seq[Coordinates] =
    neighboringCoordinatesWithValues(coordinates)
      .filter(coordinatesWithValue => filter(coordinatesWithValue.value))
      .map(_.coordinates)
