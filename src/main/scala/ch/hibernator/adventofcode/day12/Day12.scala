package ch.hibernator.adventofcode.day12

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.Direction4.Up
import ch.hibernator.adventofcode.util.{Coordinates, Direction4}
import ch.hibernator.adventofcode.util.InputOps.numRowsAndColumns
import ch.hibernator.adventofcode.util.mutable.FilledGrid

import scala.annotation.tailrec
import scala.collection.mutable

object Day12 extends SolutionBaseSimple:
  override def day: Int = 12

  override def solve(input: Seq[String]): (Long, Long) =
    val (numRows, numColumns) = input.numRowsAndColumns

    val grid =
      val gridArray = Array.fill(numRows)(Array.fill(numColumns)('0'))
      for
        row <- 0 until numRows
        column <- 0 until numColumns
      do gridArray(row)(column) = input(row)(column)
      assert(gridArray.forall(_.forall(_ != '0')))
      FilledGrid[Char](gridArray)

    // Breadth-first-search
    def findRegion(start: Coordinates): Region =
      val pointsToVisit: mutable.Queue[Coordinates] = mutable.Queue(start)
      val visited: mutable.Set[Coordinates] = mutable.LinkedHashSet(start)
      val plotValue = grid.get(start)

      while pointsToVisit.nonEmpty
      do
        val currentPoint = pointsToVisit.dequeue()
        val possibleNeighbors = grid.conditionalNeighbors(currentPoint)(_ == plotValue).filterNot(visited.contains)
        visited.addAll(possibleNeighbors)
        pointsToVisit.enqueueAll(possibleNeighbors)

      Region(visited, plotValue)

    val allRegions: mutable.Set[Region] =
      val regions: mutable.Set[Region] = mutable.LinkedHashSet()
      val visitedPlots: mutable.Set[Coordinates] = mutable.LinkedHashSet()

      for
        row <- 0 until numRows
        column <- 0 until numColumns
      do
        val coordinates = Coordinates(row, column)
        if !visitedPlots.contains(coordinates) then
          val region = findRegion(coordinates)
          visitedPlots.addAll(region.plots)
          regions.add(region)

      regions

    val result1 = allRegions.toSeq.map(_.price).sum
    println(result1)

    val result2 = allRegions.toSeq.map(_.priceSides).sum
    println(result2)

    (result1, result2)

case class Region(plots: mutable.Set[Coordinates], value: Char):
  val area: Int = plots.size

  val perimeter: Int =
    var counter = 0
    plots.foreach(counter += 4 - _.allNeighbors4.count(plots.contains))
    counter

  val price: Long = area * perimeter

  val priceSides: Long =
    val directionToSides: mutable.Map[Direction4, mutable.Buffer[mutable.Buffer[Coordinates]]] = mutable.Map(
      Direction4.Up -> mutable.Buffer(),
      Direction4.Down -> mutable.Buffer(),
      Direction4.Left -> mutable.Buffer(),
      Direction4.Right -> mutable.Buffer()
    )

    @tailrec
    def moveAlongLine(
        line: mutable.Buffer[Coordinates],
        lineDirection: Direction4,
        moveDirection: Direction4
    ): mutable.Seq[Coordinates] =
      if lineDirection == Direction4.Up || lineDirection == Direction4.Down then
        if moveDirection == Direction4.Left then
          val nextPlot = line.head.move(moveDirection)
          if plots.contains(nextPlot) then
            if !plots.contains(nextPlot.move(lineDirection)) then
              moveAlongLine(line.prepend(nextPlot), lineDirection, moveDirection)
            else line
          else line
        else if moveDirection == Direction4.Right then
          val nextPlot = line.last.move(moveDirection)
          if plots.contains(nextPlot) then
            if !plots.contains(nextPlot.move(lineDirection)) then
              moveAlongLine(line.append(nextPlot), lineDirection, moveDirection)
            else line
          else line
        else sys.error("Wrong combination")
      else if lineDirection == Direction4.Left || lineDirection == Direction4.Right then
        if moveDirection == Direction4.Up then
          val nextPlot = line.head.move(moveDirection)
          if plots.contains(nextPlot) then
            if !plots.contains(nextPlot.move(lineDirection)) then
              moveAlongLine(line.prepend(nextPlot), lineDirection, moveDirection)
            else line
          else line
        else if moveDirection == Direction4.Down then
          val nextPlot = line.last.move(moveDirection)
          if plots.contains(nextPlot) then
            if !plots.contains(nextPlot.move(lineDirection)) then
              moveAlongLine(line.append(nextPlot), lineDirection, moveDirection)
            else line
          else line
        else sys.error("Wrong combination")
      else sys.error("wrong direction")

    for plot <- plots
    do
      val newLineDirections = Direction4.values
        .map(direction => (direction, plot.move(direction)))
        .filterNot { case (direction, neighbor) =>
          plots.contains(neighbor)
        }
        .filterNot { case (direction, coordinates) =>
          directionToSides(direction).exists(_.contains(plot))
        }
        .map(_._1)

      // build lines
      for direction <- newLineDirections
      do
        val newLine: mutable.Buffer[Coordinates] = mutable.Buffer(plot)
        if direction == Up || direction == Direction4.Down then
          // look left
          moveAlongLine(newLine, direction, Direction4.Left)
          // look right
          moveAlongLine(newLine, direction, Direction4.Right)
        if direction == Direction4.Left || direction == Direction4.Right then
          // look up
          moveAlongLine(newLine, direction, Direction4.Up)
          // look down
          moveAlongLine(newLine, direction, Direction4.Down)
        directionToSides(direction).append(newLine)

    area * directionToSides.map { case (_, lines) => lines.size }.sum

  override def toString: String =
    s"Value='$value'. Area=$area. Perimeter=$perimeter. Price=$price. PriceSides=$priceSides"
