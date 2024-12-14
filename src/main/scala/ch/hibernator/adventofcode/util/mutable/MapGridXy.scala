package ch.hibernator.adventofcode.util.mutable

import ch.hibernator.adventofcode.util.CoordinatesXy

import scala.collection.mutable

class MapGridXy[T](val map: mutable.LinkedHashMap[CoordinatesXy, T]):
  def getValue(coordinates: CoordinatesXy): Option[T] = map.get(coordinates)
  def getValueStrict(coordinates: CoordinatesXy): T = map(coordinates)
  def getValue(row: Int, column: Int): Option[T] = map.get(CoordinatesXy(row, column))
  def getValueStrict(row: Int, column: Int): T = map(CoordinatesXy(row, column))
  def getNeighborCoordinates(coordinates: CoordinatesXy): Seq[CoordinatesXy] =
    getValue(coordinates).map(_ => coordinates.allNeighbors4.filter(isInGrid)).toSeq.flatten

  def isInGrid(coordinates: CoordinatesXy): Boolean = map.contains(coordinates)
