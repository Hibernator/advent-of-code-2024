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
import scala.util.control.Breaks
import scala.util.control.Breaks.break

object Day16 extends SolutionBaseSimple:
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

    // A* algorithm, also records multiple paths for the second part of the puzzle

    val stepPredecessors = mutable.Map[Step, mutable.Buffer[Step]]().withDefaultValue(mutable.Buffer())

    def cheapestPath: Step =
//      given Ordering[Step] = Ordering.fromLessThan { (step1, step2) =>
//        step1.estimatedTotalCost < step2.estimatedTotalCost
//      }

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
          val currentStep = openList.remove(0)
          if currentStep.location == endLocation then
            currentStep.costFromStart =
              currentStep.previousStep.costFromStart + currentStep.costFromNeighborStep(currentStep.previousStep)
            result = currentStep
            break()
          closedList.add(currentStep)
          val potentialNextSteps =
            Direction4.values
              .map(direction => Step(currentStep.location.move(direction), direction))
              .filterNot(step => grid.get(step.location) == Wall)
              .filterNot(step => step.location == currentStep.location)

          Breaks.breakable {
            for nextStep <- potentialNextSteps
            do
              if closedList.contains(nextStep)
              then
                break()

              val tentativeCostFromStart = currentStep.costFromStart + nextStep.costFromNeighborStep(currentStep)

              if !openList.contains(nextStep) then openList.addOne(nextStep)
              val actualNextStep = openList.find(_ == nextStep).get
              val bestCostFromStartSoFar = Option(actualNextStep.costFromStart).getOrElse(Long.MaxValue)
              if tentativeCostFromStart >= bestCostFromStartSoFar
              then
//                if tentativeCostFromStart == bestCostFromStartSoFar // alternative best path
//                then
//                  val existingPredecessors = stepPredecessors(actualNextStep)
//                  stepPredecessors.put(actualNextStep, existingPredecessors.append(currentStep))
                break() // this path is not better

              // this path is best so far

              actualNextStep.previousStep = currentStep
              actualNextStep.costFromStart = tentativeCostFromStart
              actualNextStep.estimatedCostToEnd = estimatedCostToEnd(nextStep)
          }
      }
      result

    val finalStep = cheapestPath
    val result = finalStep.costFromStart

    val onePath = finalStep.pathFromStart

    println(result)

    (result, 1L)

  given Show[Tile] = Show.fromToString

case class Step(location: Location, direction: Direction4):
  var previousStep: Step = _

  def costFromNeighborStep(neighbor: Step): Int =
    assert(neighbor.location.isNeighbor4Of(location))
    if neighbor.direction == direction then 1
    else if neighbor.direction == direction.opposite then 2001
    else 1001

  // g
  var costFromStart: Long = Long.MaxValue

  // h
  var estimatedCostToEnd: Long = _

  // f
  def estimatedTotalCost: Long = costFromStart + estimatedCostToEnd

  def pathFromStart: mutable.Buffer[Step] =
    @tailrec
    def pathFromStart(step: Step, acc: mutable.Buffer[Step]): mutable.Buffer[Step] =
      if step.previousStep == null then acc.prepend(step)
      else pathFromStart(step.previousStep, acc.prepend(step))

    pathFromStart(this, mutable.Buffer())

enum Tile(representation: Char):
  override def toString: String = representation.toString

  case Empty extends Tile('.')
  case Wall extends Tile('#')
  case Start extends Tile('S')
  case End extends Tile('E')

object Tile:
  def fromChar(char: Char): Tile =
    char match
      case '.' => Empty
      case '#' => Wall
      case 'S' => Start
      case 'E' => End
