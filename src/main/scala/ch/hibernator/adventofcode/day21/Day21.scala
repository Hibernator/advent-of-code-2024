package ch.hibernator.adventofcode.day21

import cats.Show
import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.{Direction4, Direction8}
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

      // Reconstructs all the paths
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
    def findAllShortestPathsOfCode(code: String): Seq[Seq[LocationWithPush]] =
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

    // A naive algorithm to find all shortest combinations of presses on the last directional keyboard
    // It is inefficient and can't be scaled to longer keyboard chains
    def findShortestCombinationSizeForCode(code: String): Int =
      val firstArrowPadCombinations = findAllShortestPathsOfCode(code).map(pathToArrows).sortBy(_.size)
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

//    val result1 = input.map(code => code.take(3).toInt * findShortestCombinationSizeForCode(code)).sum
//    println(result1)

    // Part 2
    // Part 1 solution is not scalable because the number of shortest paths per robot grows too fast
    // Part 1 solution is also completely overengineered
    // Part 1 solution already takes a while with 4 keypads only. Even memoization wouldn't help

    /*
      Greedy algorithm for finding ONE shortest path:
      - on both numpad and arrow pad at most one turn is needed for every start and end
        - this minimizes the number of travels to A on arrow pad
        - for example zig-zag from 7 to 3 (74523) on numpad would result in too long path on arrow pad
        - path with one turn only (78963) would be more efficient on arrow pad
      - the empty corner has to be taken into account and avoided
        - on numpad, when going from up-left to bottom-right, right movement needs to be preferred
        - on numpad, when going from down-right to top left, up movement has to be preferred
        - on arrow pad, when going in up-right direction, right movement is preferred
        - on arrow pad, when going in down-left direction, down movement is preferred
      - for other diagonal directions, there are also preferred movements (even though it doesn't look like it at first)
     */

    // Finds a path on numpad between two buttons
    def numpadPath(start: NumPadValue, end: NumPadValue): Seq[Direction4] =
      if start == end then Nil
      else
        val startLocation = numPad.findValueLocation(start)
        val endLocation = numPad.findValueLocation(end)
        val direction = startLocation.direction8To(endLocation)
        val verticalDistance = startLocation.verticalDistanceTo(endLocation)
        val horizontalDistance = startLocation.horizontalDistanceTo(endLocation)
        direction match
          case Direction8.Left  => Seq.fill(horizontalDistance)(Direction4.Left)
          case Direction8.Right => Seq.fill(horizontalDistance)(Direction4.Right)
          case Direction8.Up    => Seq.fill(verticalDistance)(Direction4.Up)
          case Direction8.Down  => Seq.fill(verticalDistance)(Direction4.Down)
          case Direction8.DownLeft =>
            Seq.fill(horizontalDistance)(Direction4.Left) ++ Seq.fill(verticalDistance)(Direction4.Down)
          case Direction8.DownRight if NumPadValue.LeftRow.contains(start) && NumPadValue.BottomRow.contains(end) =>
            Seq.fill(horizontalDistance)(Direction4.Right) ++ Seq.fill(verticalDistance)(Direction4.Down)
          case Direction8.DownRight =>
            Seq.fill(verticalDistance)(Direction4.Down) ++ Seq.fill(horizontalDistance)(Direction4.Right)
          case Direction8.UpLeft if NumPadValue.BottomRow.contains(start) && NumPadValue.LeftRow.contains(end) =>
            Seq.fill(verticalDistance)(Direction4.Up) ++ Seq.fill(horizontalDistance)(Direction4.Left)
          case Direction8.UpLeft =>
            Seq.fill(horizontalDistance)(Direction4.Left) ++ Seq.fill(verticalDistance)(Direction4.Up)
          case Direction8.UpRight =>
            Seq.fill(verticalDistance)(Direction4.Up) ++ Seq.fill(horizontalDistance)(Direction4.Right)

    // Finds a path on directional keyboard between two buttons
    def arrowPadPath(start: ArrowPadValue, end: ArrowPadValue): Seq[Direction4] =
      if start == end then Nil
      else
        val startLocation = arrowPad.findValueLocation(start)
        val endLocation = arrowPad.findValueLocation(end)
        val direction = startLocation.direction8To(endLocation)
        val verticalDistance = startLocation.verticalDistanceTo(endLocation)
        val horizontalDistance = startLocation.horizontalDistanceTo(endLocation)
        direction match
          case Direction8.Left  => Seq.fill(horizontalDistance)(Direction4.Left)
          case Direction8.Right => Seq.fill(horizontalDistance)(Direction4.Right)
          case Direction8.Up    => Seq.fill(verticalDistance)(Direction4.Up)
          case Direction8.Down  => Seq.fill(verticalDistance)(Direction4.Down)
          case Direction8.DownLeft if end == ArrowPadValue.Left =>
            Seq.fill(verticalDistance)(Direction4.Down) ++ Seq.fill(horizontalDistance)(Direction4.Left)
          case Direction8.DownLeft =>
            Seq.fill(horizontalDistance)(Direction4.Left) ++ Seq.fill(verticalDistance)(Direction4.Down)
          case Direction8.DownRight =>
            Seq.fill(verticalDistance)(Direction4.Down) ++ Seq.fill(horizontalDistance)(Direction4.Right)
          case Direction8.UpLeft =>
            Seq.fill(horizontalDistance)(Direction4.Left) ++ Seq.fill(verticalDistance)(Direction4.Up)
          case Direction8.UpRight if start == ArrowPadValue.Left =>
            Seq.fill(horizontalDistance)(Direction4.Right) ++ Seq.fill(verticalDistance)(Direction4.Up)
          case Direction8.UpRight =>
            Seq.fill(verticalDistance)(Direction4.Up) ++ Seq.fill(horizontalDistance)(Direction4.Right)

    // Memoize all paths
    val numpadPaths = mutable.Map[(NumPadValue, NumPadValue), Seq[Direction4]]()
    val arrowPadPaths = mutable.Map[(ArrowPadValue, ArrowPadValue), Seq[Direction4]]()

    for
      start <- NumPadValue.values
      end <- NumPadValue.values
    do numpadPaths.addOne((start, end), numpadPath(start, end))

    for
      start <- ArrowPadValue.values
      end <- ArrowPadValue.values
    do arrowPadPaths.addOne((start, end), arrowPadPath(start, end))

    // Returns a sequence of presses on directional keyboard that needs to be pressed in order to enter the numeric code
    def codeToArrowButtons(code: String): Seq[ArrowPadValue] =
      val acc = mutable.Buffer[ArrowPadValue]()
      s"A$code".sliding(2).foreach { pair =>
        val (start, end) = (NumPadValue.fromChar(pair.head), NumPadValue.fromChar(pair.last))
        acc.appendAll(numpadPaths(start, end).map(ArrowPadValue.fromDirection))
        acc.append(ArrowPadValue.A)
      }
      acc.toSeq

    // Returns a sequence of directional keyboard presses that needs to be pushed on the next directional keyboard
    // to navigate between two locations on the current keyboard
    def arrowsToArrowButtons(start: ArrowPadValue, end: ArrowPadValue): Seq[ArrowPadValue] =
      val acc = mutable.Buffer[ArrowPadValue]()
      Seq(start, end)
        .sliding(2)
        .foreach { pair =>
          val (start, end) = (pair.head, pair.last)
          acc.appendAll(arrowPadPaths(start, end).map(ArrowPadValue.fromDirection))
          acc.append(ArrowPadValue.A)
        }
      acc.toSeq

    // Memoize all the moves on directional keyboard
    val arrowsToArrowButtonsMap = mutable.Map[(ArrowPadValue, ArrowPadValue), Seq[ArrowPadValue]]()
    for
      start <- ArrowPadValue.values.filterNot(_ == ArrowPadValue.Empty)
      end <- ArrowPadValue.values.filterNot(_ == ArrowPadValue.Empty)
    do arrowsToArrowButtonsMap.addOne((start, end), arrowsToArrowButtons(start, end))

    // Calculates the amount of presses on the final directional keyboard in order to enter the numeric code
    // The amount of presses is large, therefore it can't be represented as sequence of presses
    // We can only calculate the number of presses, not the actual sequence
    // It's a depth-first search (from numeric keyboard to the final directional keyboard)
    // The intermediate results are memoized, otherwise we'd be exploring same paths over and over again
    // Potential problem, the inner numPressesForState function is not tail-recursive, so the scalability is limited
    def codeToArrowPadPresses(code: String, numDirectionalKeyboards: Int): Long =
      val firstArrows = codeToArrowButtons(code)
      var pressesAcc: Long = 0L
      val toExplore = mutable.Buffer[StateToExplore]()

      toExplore.append(StateToExplore(1, mutable.Buffer.from(firstArrows), startedExploring = false))

      val level1Presses = firstArrows
      val level2Presses = mutable.Buffer[ArrowPadValue]()
      val finalPresses = mutable.Buffer[ArrowPadValue]()

      val stateToPresses = mutable.Map[StateToExplore, Long]()

      def numPressesForState(state: StateToExplore): Long =
        val maybePresses = stateToPresses.get(state)
        maybePresses match
          case Some(presses) => presses
          case None =>
            val presses = {
              val level = state.level
              val pathToExplore = state.path
              if level == numDirectionalKeyboards then pathToExplore.size.toLong
              else if pathToExplore.sizeIs == 1
              then
                if !state.startedExploring then 1L
                else 0L
              else
                val newLevel = level + 1
                if !state.startedExploring then
                  val (start, end) = (ArrowPadValue.A, pathToExplore.head)
                  val newPath = mutable.Buffer.from(arrowsToArrowButtonsMap(start, end))
                  numPressesForState(StateToExplore(newLevel, newPath, startedExploring = false)) + numPressesForState(
                    state.copy(startedExploring = true)
                  )
                else
                  val (start, end) = (pathToExplore.head, pathToExplore.tail.head)
                  val newPath = mutable.Buffer.from(arrowsToArrowButtonsMap(start, end))
                  numPressesForState(StateToExplore(newLevel, newPath, startedExploring = false)) + numPressesForState(
                    state.copy(path = pathToExplore.tail)
                  )
            }
            stateToPresses.addOne(state, presses)
            presses

      numPressesForState(StateToExplore(1, mutable.Buffer.from(firstArrows), startedExploring = false))

    val result1 = input.map { code =>
      val arrowPresses = codeToArrowPadPresses(code, 3)
      val codeNumber = code.init.toInt
      arrowPresses * codeNumber.toLong
    }.sum
    println(result1)

    val result2 =
      input
        .map: code =>
          val arrowPresses = codeToArrowPadPresses(code, 26)
          val codeNumber = code.init.toInt
          arrowPresses * codeNumber.toLong
        .sum
    (result1, result2)

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

  val LeftRow: Set[NumPadValue] = Set(NumPadValue.Seven, NumPadValue.Four, NumPadValue.One)

  val BottomRow: Set[NumPadValue] = Set(NumPadValue.Zero, NumPadValue.A)

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

  val TopRow: Set[ArrowPadValue] = Set(ArrowPadValue.Up, ArrowPadValue.A)

case class LocationWithDistanceFromStart(location: Location, distance: Int)

object LocationWithDistanceFromStart:
  // Items with the lowest distance have the highest priority
  given Ordering[LocationWithDistanceFromStart] = Ordering.fromLessThan((loc1, loc2) => loc1.distance > loc2.distance)

case class LocationWithPush(location: Location, push: Boolean)

case class StateToExplore(level: Int, path: mutable.Buffer[ArrowPadValue], var startedExploring: Boolean)
