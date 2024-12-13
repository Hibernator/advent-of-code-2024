package ch.hibernator.adventofcode.util.mutable

import ch.hibernator.adventofcode.util.{Coordinates, Direction4}

import scala.collection.mutable

class MapGrid[T](map: mutable.LinkedHashMap[Coordinates, T]):
  def getValue(coordinates: Coordinates): Option[T] = map.get(coordinates)
  def getValueStrict(coordinates: Coordinates): T = map(coordinates)
  def getValue(row: Int, column: Int): Option[T] = map.get(Coordinates(row, column))
  def getValueStrict(row: Int, column: Int): T = map(Coordinates(row, column))
  def getNeighborCoordinates(coordinates: Coordinates): Seq[Coordinates] =
    getValue(coordinates).map(_ => coordinates.allNeighbors4.filter(isInGrid)).toSeq.flatten

  def isInGrid(coordinates: Coordinates): Boolean = map.contains(coordinates)
