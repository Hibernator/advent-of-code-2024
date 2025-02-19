package ch.hibernator.adventofcode.day21

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.Direction4
import ch.hibernator.adventofcode.util.mutable.FullGridXy
import ch.hibernator.adventofcode.util.mutable.FullGridXy.Location

import scala.annotation.tailrec
import scala.collection.mutable

object Day21 extends SolutionBaseSimple:
  override def day: Int = 21

  override def solve(input: Seq[String]): (Long, Long) =
    val codes = input.map(_.take(3))

    given Show[NumPadValue] = Show.fromToString
    given Show[ArrowPadValue] = Show.fromToString

    val numPad = FullGridXy[Blockable](Array.fill(3)(Array.fill(4)(NumPadValue.Empty)))
    val arrowPad = FullGridXy[Blockable](Array.fill(3)(Array.fill(2)(ArrowPadValue.Empty)))

    numPad.set(0, 0, NumPadValue.Seven)
    numPad.set(1, 0, NumPadValue.Eight)
    numPad.set(2, 0, NumPadValue.Nine)
    numPad.set(0, 1, NumPadValue.Four)
    numPad.set(1, 1, NumPadValue.Five)
    numPad.set(2, 1, NumPadValue.Six)
    numPad.set(0, 2, NumPadValue.One)
    numPad.set(1, 2, NumPadValue.Two)
    numPad.set(2, 2, NumPadValue.Three)
    numPad.set(1, 3, NumPadValue.Zero)
    numPad.set(2, 3, NumPadValue.A)

    arrowPad.set(1, 0, ArrowPadValue.Up)
    arrowPad.set(2, 0, ArrowPadValue.A)
    arrowPad.set(0, 1, ArrowPadValue.Left)
    arrowPad.set(1, 1, ArrowPadValue.Down)
    arrowPad.set(2, 1, ArrowPadValue.Right)

    // Dijkstra algorithm with the following changes
    // - only new nodes are put into frontier - priority queue
    // - since we know the Manhattan distance from start to end,
    // we don't process any nodes with distance from start higher than that
    // - all shortest paths are found by the means of keeping track of previous locations on the path
    // - shortest paths are reconstructed in the second step
    def findShortestPaths(start: Location, end: Location, grid: FullGridXy[Blockable]): Seq[Seq[Location]] =
      val distanceFromStartToEnd = start.manhattanDistanceTo(end)
      val frontier = mutable.PriorityQueue(LocationWithDistanceFromStart(start, 0))
      val locationByDistanceFromStart = mutable.Map[Location, Int]().withDefaultValue(Int.MaxValue)
      val locationByPrevious =
        mutable.Map[Location, mutable.Set[Location]]().withDefaultValue(mutable.Set[Location]())

      while frontier.nonEmpty do
        val locationToProcess = frontier.dequeue()
        val neighbors =
          grid
            .neighboringLocations(locationToProcess.location)
            .filterNot(location =>
              grid.get(location).blocked || start.manhattanDistanceTo(location) > distanceFromStartToEnd
            )
            .filter(location =>
              if start.manhattanDistanceTo(location) == distanceFromStartToEnd then location == end else true
            )
        neighbors.foreach { neighbor =>
          val alternativeDistance = locationToProcess.distance + 1
          if alternativeDistance <= locationByDistanceFromStart(neighbor) then
            if alternativeDistance < locationByDistanceFromStart(neighbor) then
              locationByPrevious.addOne(neighbor, mutable.Set(locationToProcess.location))
            else locationByPrevious.addOne(neighbor, locationByPrevious(neighbor).addOne(locationToProcess.location))
            locationByDistanceFromStart.addOne(neighbor, alternativeDistance)
            frontier.addOne(LocationWithDistanceFromStart(neighbor, alternativeDistance))
        }

      def constructPathsFromEnd: mutable.Buffer[mutable.Buffer[Location]] =

        @tailrec
        def pathsFromEnd(acc: mutable.Buffer[mutable.Buffer[Location]]): mutable.Buffer[mutable.Buffer[Location]] =
          if acc.forall(_.head == start) then acc
          else
            val newAcc = mutable.Buffer[mutable.Buffer[Location]]()
            acc.foreach { path =>
              if path.head != start then
                val previousLocations = locationByPrevious(path.head)
                if previousLocations.sizeIs == 1 then
                  path.prepend(previousLocations.head)
                  newAcc.append(path)
                else
                  val (head, rest) = (previousLocations.head, previousLocations.tail)
                  rest.foreach { previousLocation =>
                    val newPath = mutable.Buffer.from(path)
                    newPath.prepend(previousLocation)
                    newAcc.append(newPath)
                  }
                  path.prepend(head)
                  newAcc.append(path)
              else newAcc.append(path)
            }
            pathsFromEnd(newAcc)

        pathsFromEnd(mutable.Buffer(mutable.Buffer(end)))

      constructPathsFromEnd.map(_.toSeq).toSeq

    def findShortestPathsOnNumpad(start: NumPadValue, end: NumPadValue): Seq[Seq[Location]] =
      findShortestPaths(numPad.findValueLocation(start), numPad.findValueLocation(end), numPad)

    def findShortestPathsOnArrowpad(start: ArrowPadValue, end: ArrowPadValue): Seq[Seq[Location]] =
      findShortestPaths(arrowPad.findValueLocation(start), arrowPad.findValueLocation(end), arrowPad)

    // finds all paths on numpad that can be taken to enter the code
    // the push field specifies whether the button needs to be pushed (part of the code)
    def findShortestPathsOfCode(code: String): Seq[Seq[LocationWithPush]] =
      val pathsBetweenNumbers =
        s"A$code"
          .sliding(2)
          .map(values => (NumPadValue.fromChar(values.head), NumPadValue.fromChar(values.last)))
          .map { (start, end) =>
            findShortestPathsOnNumpad(start, end)
          }
          .toSeq
      for
        pathsForPair1 <- pathsBetweenNumbers.head
        pathsforPair2 <- pathsBetweenNumbers(1)
        pathsforPair3 <- pathsBetweenNumbers(2)
        pathsforPair4 <- pathsBetweenNumbers.last
      yield (pathsForPair1.init.map(LocationWithPush(_, false)) :+ LocationWithPush(pathsForPair1.last, true)) ++
        (pathsforPair2.tail.init.map(LocationWithPush(_, false)) :+ LocationWithPush(pathsforPair2.last, true)) ++
        (pathsforPair3.tail.init.map(LocationWithPush(_, false)) :+ LocationWithPush(pathsforPair3.last, true)) ++
        (pathsforPair4.tail.init.map(LocationWithPush(_, false)) :+ LocationWithPush(pathsforPair4.last, true))

    // converts the path (represented by locations) into the sequence of buttons that need to be pressed on arrow pad
    def pathToArrows(codePath: Seq[LocationWithPush]): Seq[ArrowPadValue] =
      codePath
        .sliding(2)
        .flatMap { pair =>
          val (start, end) = (pair.head, pair.last)
          if start.location == end.location then Seq(ArrowPadValue.A)
          else
            assert(start.location.isNeighbor4Of(end.location))
            val direction = end.location.direction4FromNeighbor(start.location)
            val directionButton = ArrowPadValue.fromDirection(direction)
            if end.push then Seq(directionButton, ArrowPadValue.A) else Seq(directionButton)
        }
        .toSeq

    // finds all paths on arrow pad that enter the provided sequence of arrows and A
    def findShortestPathsOfArrows(arrows: Seq[ArrowPadValue]): Seq[Seq[LocationWithPush]] =
      val pathsBetweenArrows =
        (ArrowPadValue.A +: arrows)
          .sliding(2)
          .map { pair =>
            findShortestPathsOnArrowpad(pair.head, pair.last)
          }
          .toSeq
      val pathsAcc = mutable.Buffer[mutable.Buffer[LocationWithPush]]()
      pathsBetweenArrows.head.foreach { pathPart =>
        pathsAcc.append(
          mutable.Buffer
            .from(pathPart.init.map(LocationWithPush(_, false)))
            .append(LocationWithPush(pathPart.last, true))
        )
      }

      @tailrec
      def extendPaths(toProcess: Seq[Seq[Seq[Location]]]): Unit =
        if toProcess.isEmpty then ()
        else
          val nextPathsPartsToProcess = toProcess.head
          val (first, rest) = (nextPathsPartsToProcess.head, nextPathsPartsToProcess.tail)
          val newPaths = for
            existingPath <- pathsAcc
            partToAdd <- rest
          yield
            val newPath = mutable.Buffer.from(existingPath)
            if partToAdd.sizeIs > 1 then newPath.appendAll(partToAdd.tail.init.map(LocationWithPush(_, false)))
            newPath.append(LocationWithPush(partToAdd.last, true))
            newPath

          pathsAcc.foreach { path =>
            if first.sizeIs > 1 then path.appendAll(first.tail.init.map(LocationWithPush(_, false)))
            path
              .append(LocationWithPush(first.last, true))
          }
          pathsAcc.appendAll(newPaths)
          extendPaths(toProcess.tail)

      extendPaths(pathsBetweenArrows.tail)

      pathsAcc.map(_.toSeq).toSeq

    def findShortestCombinationSizeForCode(code: String): Int =
      val firstArrowPadCombinations = findShortestPathsOfCode(code).map(pathToArrows).sortBy(_.size)
      val firstArrowPadCombinationsShortestSize = firstArrowPadCombinations.head.size
      val shortestFirstArrowPadCombinations =
        firstArrowPadCombinations.filter(_.size == firstArrowPadCombinationsShortestSize)

      val secondArrowPadCombinations =
        shortestFirstArrowPadCombinations.flatMap(findShortestPathsOfArrows).map(pathToArrows).sortBy(_.size)
      val secondArrowPadCombinationsShortestSize = secondArrowPadCombinations.head.size
      val shortestSecondArrowPadCombinations =
        secondArrowPadCombinations.filter(_.size == secondArrowPadCombinationsShortestSize)

      val thirdArrowPadCombinations =
        shortestSecondArrowPadCombinations.flatMap(findShortestPathsOfArrows).map(pathToArrows).sortBy(_.size)
      val thirdArrowPadCombinationsShortestSize = thirdArrowPadCombinations.head.size
      thirdArrowPadCombinationsShortestSize

    val result1 = input.map(code => code.take(3).toInt * findShortestCombinationSizeForCode(code)).sum
    println(result1)

    (result1, 1L)

abstract class Blockable(val blocked: Boolean)

enum NumPadValue(val representation: Char, blocked: Boolean = false) extends Blockable(blocked):
  override def toString: String = representation.toString

  case Empty extends NumPadValue('X', blocked = true)
  case A extends NumPadValue('A')
  case Zero extends NumPadValue('0')
  case One extends NumPadValue('1')
  case Two extends NumPadValue('2')
  case Three extends NumPadValue('3')
  case Four extends NumPadValue('4')
  case Five extends NumPadValue('5')
  case Six extends NumPadValue('6')
  case Seven extends NumPadValue('7')
  case Eight extends NumPadValue('8')
  case Nine extends NumPadValue('9')

object NumPadValue:
  def fromChar(value: Char): NumPadValue = NumPadValue.values.find(_.representation == value).get

enum ArrowPadValue(val representation: Char, blocked: Boolean = false) extends Blockable(blocked):
  override def toString: String = representation.toString

  case Empty extends ArrowPadValue('X', blocked = true)
  case A extends ArrowPadValue('A')
  case Left extends ArrowPadValue('<')
  case Right extends ArrowPadValue('>')
  case Up extends ArrowPadValue('^')
  case Down extends ArrowPadValue('v')

object ArrowPadValue:
  def fromChar(value: Char): ArrowPadValue = ArrowPadValue.values.find(_.representation == value).get

  def fromDirection(direction: Direction4): ArrowPadValue =
    direction match
      case Direction4.Up    => ArrowPadValue.Up
      case Direction4.Down  => ArrowPadValue.Down
      case Direction4.Left  => ArrowPadValue.Left
      case Direction4.Right => ArrowPadValue.Right

case class LocationWithDistanceFromStart(location: Location, distance: Int)

object LocationWithDistanceFromStart:
  // Items with the lowest distance have the highest priority
  given Ordering[LocationWithDistanceFromStart] = Ordering.fromLessThan((loc1, loc2) => loc1.distance > loc2.distance)

case class LocationWithPush(location: Location, push: Boolean)
