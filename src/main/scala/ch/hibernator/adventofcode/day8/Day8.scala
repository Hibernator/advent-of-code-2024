package ch.hibernator.adventofcode.day8

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.collection.mutable

object Day8 extends SolutionBaseSimple:
  override def day: Int = 8

  override def solve(input: Seq[String]): (Long, Long) =
    val numRows = input.size
    val numColumns = input.head.length
    val antennaCoordinatesByFrequency: mutable.Map[Char, Set[Coordinates]] = mutable.Map().withDefaultValue(Set())
    for
      row <- 0 until numRows
      column <- 0 until numColumns
    do
      val frequency = input(row)(column)
      if frequency != '.' then
        val antennas = antennaCoordinatesByFrequency(frequency)
        antennaCoordinatesByFrequency.addOne(frequency, antennas + Coordinates(row, column))

    def isInsideGrid(location: Coordinates) =
      location.row >= 0 && location.row < numRows && location.column >= 0 && location.column < numColumns

    val antinodes: mutable.Set[Coordinates] = mutable.Set[Coordinates]()

    antennaCoordinatesByFrequency.values.filter(_.sizeIs > 1).foreach { antennas =>
      val antennaPairs = antennas.toSeq.combinations(2).toSeq
      antennaPairs.foreach { antennaPair =>
        val (firstAntenna, secondAntenna) = (antennaPair.head, antennaPair.last)
        val fromFirstToSecond = Vector.between(firstAntenna, secondAntenna)
        val fromSecondToFirst = fromFirstToSecond.reverse
        val antinodeCloseToFirst = firstAntenna.move(fromSecondToFirst)
        val antinodeCloseToSecond = secondAntenna.move(fromFirstToSecond)
        if isInsideGrid(antinodeCloseToFirst) then antinodes += antinodeCloseToFirst
        if isInsideGrid(antinodeCloseToSecond) then antinodes += antinodeCloseToSecond
      }
    }

    val antinodesWithResonantHarmonics: mutable.Set[Coordinates] = mutable.Set[Coordinates]()
    antennaCoordinatesByFrequency.values.filter(_.sizeIs > 1).foreach { antennas =>
      val antennaPairs = antennas.toSeq.combinations(2).toSeq
      antennaPairs.foreach { antennaPair =>
        val (firstAntenna, secondAntenna) = (antennaPair.head, antennaPair.last)

        // Assume that no two antennas of the same frequency are in the same row or column
        val fromFirstToSecond = Vector.between(firstAntenna, secondAntenna)
        val fromSecondToFirst = fromFirstToSecond.reverse

        var firstAntennaAntinode = firstAntenna
        while isInsideGrid(firstAntennaAntinode)
        do
          antinodesWithResonantHarmonics += firstAntennaAntinode
          firstAntennaAntinode = firstAntennaAntinode.move(fromSecondToFirst)

        var secondAntennaAntinode = secondAntenna
        while isInsideGrid(secondAntennaAntinode)
        do
          antinodesWithResonantHarmonics += secondAntennaAntinode
          secondAntennaAntinode = secondAntennaAntinode.move(fromFirstToSecond)
      }
    }

    (antinodes.size, antinodesWithResonantHarmonics.size)
end Day8

case class Coordinates(row: Int, column: Int):
  def move(vector: Vector): Coordinates =
    Coordinates(row + vector.rowChange, column + vector.columnChange)

case class Vector(rowChange: Int, columnChange: Int):
  def reverse: Vector = Vector(-rowChange, -columnChange)

object Vector:
  def between(start: Coordinates, destination: Coordinates): Vector =
    Vector(destination.row - start.row, destination.column - start.column)
