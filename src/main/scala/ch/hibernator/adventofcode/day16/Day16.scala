package ch.hibernator.adventofcode.day16

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day16.Tile.{End, Start, Wall}
import ch.hibernator.adventofcode.util.Direction4
import ch.hibernator.adventofcode.util.Direction4.{Down, Up}
import ch.hibernator.adventofcode.util.InputOps.numRowsAndColumns
import ch.hibernator.adventofcode.util.mutable.FullGridXy
import ch.hibernator.adventofcode.util.mutable.FullGridXy.Location

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.uninitialized
import scala.util.control.Breaks
import scala.util.control.Breaks.break

object Day16 extends SolutionBaseSimple:
  private val bogusLocation = Location(50_000, 50_000)

  override def day: Int = 16

  override def solve(input: Seq[String]): (Long, Long) =
    val (numRows, numColumns) = input.numRowsAndColumns
    val grid = {
      val innerArray = Array.fill(numColumns)(Array.fill(numRows)(Tile.Empty))
      val actualGrid = FullGridXy[Tile](innerArray)
      for
        x <- 0 until numColumns
        y <- 0 until numRows
      do actualGrid.set(x, y, Tile.fromChar(input(y)(x)))
      actualGrid
    }

    println(grid.asText)

    val startLocation = grid.findValueLocation(Start)
    val endLocation = grid.findValueLocation(End)

    // heuristic function for A* algorithm
    def estimatedCostToEnd(step: Step): Long =
      val location = step.location
      if location == endLocation then 0L
      else
        val distance = endLocation manhattanDistanceTo location
        step.direction match
          case Up =>
            if location.x == endLocation.x then distance else 1000 + distance
          case Down =>
            if location.y == endLocation.y then distance + 1000 else distance + 2000
          case Direction4.Left =>
            if location.x == endLocation.x then distance + 1000 else distance + 2000
          case Direction4.Right =>
            if location.y == endLocation.y then distance else distance + 1000

    // A* algorithm, finds all shortest paths for the second part of the puzzle
    // Usually, the algorithm finds one shortest path, but here all of them are needed
    // For each step, keeps track of the previous steps
    def cheapestPath: Step =
      given Ordering[Step] = Ordering.fromLessThan { (step1, step2) =>
        step1.costFromStart < step2.costFromStart
      }
      val startStep = Step(startLocation, Direction4.Right)
      startStep.costFromStart = 0L
      startStep.estimatedCostToEnd = estimatedCostToEnd(startStep)
      val openList = mutable.IndexedBuffer[Step](startStep)
      val closedList = mutable.LinkedHashSet[Step]()
      var result: Step = null

      Breaks.breakable {
        while openList.nonEmpty
        do
          openList.sortInPlace()
          println(s"Open list")
          openList.foreach(item => println(s"${item.location}, ${item.costFromStart}"))
          val currentStep = openList.remove(0)
          // check if path from any existing closed steps yields the same result
          closedList
            .find { pastStep =>
              !currentStep.previousSteps.contains(pastStep) &&
              pastStep.location.isNeighbor4Of(currentStep.location) &&
              pastStep.costFromStart + currentStep.costFromNeighborStep(pastStep) == currentStep.costFromStart
            }
            .foreach { pastStep =>
              currentStep.previousSteps.append(pastStep)
            }
          closedList.add(currentStep)
          // check if the finish was reached
          if currentStep.location == endLocation then
            currentStep.costFromStart = currentStep.previousSteps.head.costFromStart + currentStep.costFromNeighborStep(
              currentStep.previousSteps.head
            )
            if result == null then result = currentStep
//            break() // we still need to find other shortest paths
          val potentialNextSteps =
            Direction4.values
              .map(direction => Step(currentStep.location.move(direction), direction))
              .filterNot(step => grid.get(step.location) == Wall)
              .filterNot { step =>
                // first step doesn't have a previous step, bogusLocation is needed
                step.location == currentStep.previousSteps.headOption.map(_.location).getOrElse(bogusLocation)
              }

          Breaks.breakable {
            for nextStep <- potentialNextSteps
            do
              val tentativeCostFromStart = currentStep.costFromStart + nextStep.costFromNeighborStep(currentStep)
              if closedList.contains(nextStep)
              then
                val actualNextStep = closedList.find(_ == nextStep).get
                val bestCostFromStartSoFar = actualNextStep.costFromStart
                break()

              if !openList.contains(nextStep) then openList.addOne(nextStep)
              val actualNextStep = openList.find(_ == nextStep).get
              val bestCostFromStartSoFar = Option(actualNextStep.costFromStart).getOrElse(Long.MaxValue)
              if tentativeCostFromStart >= bestCostFromStartSoFar then break() // this path is not better

              // this path is best so far
              actualNextStep.previousSteps.append(currentStep)
              actualNextStep.costFromStart = tentativeCostFromStart
              actualNextStep.estimatedCostToEnd = estimatedCostToEnd(nextStep)
          }
      }
      result

    val finalStep = cheapestPath
    val result = finalStep.costFromStart

    val onePath = finalStep.pathFromStart

    println(result)
    println(s"Path length = ${onePath.size}")

    val shortestPathsLocations = finalStep.allPathsFromStart.map(_.location).toSet
    shortestPathsLocations.foreach { location =>
      grid.set(location, Tile.Path)
    }
    println(grid.asText)

    // The calculated result is actually higher than the real result
    val result2 = shortestPathsLocations.size

    (result, result2)

  given Show[Tile] = Show.fromToString

case class Step(location: Location, direction: Direction4):
  var previousSteps: mutable.Buffer[Step] = mutable.Buffer()

  def costFromNeighborStep(neighbor: Step): Int =
    assert(neighbor.location.isNeighbor4Of(location))
    if neighbor.direction == direction then 1
    else if neighbor.direction == direction.opposite then 2001
    else 1001

  // g
  var costFromStart: Long = Long.MaxValue

  // h
  var estimatedCostToEnd: Long = uninitialized

  // f
  def estimatedTotalCost: Long = costFromStart + estimatedCostToEnd

  def pathFromStart: mutable.Buffer[Step] =
    @tailrec
    def pathFromStart(step: Step, acc: mutable.Buffer[Step]): mutable.Buffer[Step] =
      if step.previousSteps.isEmpty then acc.prepend(step)
      else pathFromStart(step.previousSteps.head, acc.prepend(step))

    pathFromStart(this, mutable.Buffer())

  def allPathsFromStart: mutable.Buffer[Step] =
    @tailrec
    def pathFromStart(step: Step, acc: mutable.Buffer[Step]): mutable.Buffer[Step] =
      if step.previousSteps.isEmpty then acc.append(step)
      else if step.previousSteps.sizeIs == 1 then pathFromStart(step.previousSteps.head, acc.append(step))
      else
        acc.append(step)
        acc.appendAll(step.previousSteps.head.allPathsFromStart)
        acc.appendAll(step.previousSteps(1).allPathsFromStart)

    pathFromStart(this, mutable.Buffer())

enum Tile(representation: Char):
  override def toString: String = representation.toString

  case Empty extends Tile('.')
  case Wall extends Tile('#')
  case Start extends Tile('S')
  case End extends Tile('E')
  case Path extends Tile('O')

object Tile:
  def fromChar(char: Char): Tile =
    char match
      case '.' => Empty
      case '#' => Wall
      case 'S' => Start
      case 'E' => End
