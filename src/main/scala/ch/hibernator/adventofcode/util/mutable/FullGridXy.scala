package ch.hibernator.adventofcode.util.mutable

import cats.Show
import cats.implicits.toShow
import cats.syntax.all.*
import ch.hibernator.adventofcode.util.{Direction4, Direction8}
import ch.hibernator.adventofcode.util.mutable.FullGridXy.{Location, LocationWithValue}

/** Array-backed grid.<br/><br/>
  *
  * X axis is horizontal, Y axis is vertical.<br/> [0,0] is in the top left corner.<br/> Each array represents a column.
  *
  * @param grid
  *   array of arrays
  * @tparam T
  *   type of stored value
  */
class FullGridXy[T](grid: Array[Array[T]]):
  val numRows: Int = grid.head.length
  val numColumns: Int = grid.length
  val maxX: Int = numColumns - 1
  val maxY: Int = numRows - 1

  def get(x: Int, y: Int): T = grid(x)(y)
  def get(location: Location): T = get(location.x, location.y)
  def set(x: Int, y: Int, value: T): Unit = grid(x)(y) = value
  def set(location: Location, value: T): Unit = set(location.x, location.y, value)

  def neighboringLocations(location: Location): Seq[Location] =
    location.allNeighbors4.filter(isWithinBorders)
  def neighboringLocationsWithValues(location: Location): Seq[LocationWithValue[T]] =
    neighboringLocations(location).map(location => LocationWithValue(location, get(location)))

  def isWithinBorders(location: Location): Boolean = isWithinBorders(location.x, location.y)

  def isWithinBorders(x: Int, y: Int): Boolean = x > -1 && y > -1 && x < numColumns && y < numRows

  def conditionalNeighbors(location: Location)(
      filter: T => Boolean
  ): Seq[Location] =
    neighboringLocationsWithValues(location)
      .filter(locationWithValue => filter(locationWithValue.value))
      .map(_.location)

  def findValueLocation(value: T): Location =
    val x = grid.indexWhere(_.contains(value))
    val y = grid(x).indexOf(value)
    Location(x, y)

  def findValueLocationOpt(value: T): Option[Location] =
    val location = findValueLocation(value)
    Option.when(isWithinBorders(location))(location)

  def getNeighborLocationValue(location: Location, direction: Direction4): Option[LocationWithValue[T]] =
    val newLocation = location.move(direction)
    Option.when(isWithinBorders(newLocation))(LocationWithValue(newLocation, get(newLocation)))

  def findLocationsWithValue(value: T): Seq[Location] =
    for
      x <- 0 to maxX
      y <- 0 to maxY
      location <- Option.when(get(x, y) == value)(Location(x, y))
    yield location

  def copy: FullGridXy[T] =
    val newInnerArray = Array.fill(numColumns)(Array.fill[Any](numRows)(null))
    for
      x <- 0 to maxX
      y <- 0 to maxY
    do newInnerArray(x)(y) = get(x, y)
    FullGridXy(newInnerArray.asInstanceOf[Array[Array[T]]])

  def asText(using Show[T]): String =
    val builder = new StringBuilder
    for
      y <- 0 to maxY
      x <- 0 to maxX
    do
      builder.append(get(x, y).show)
      if x == maxX then builder.append("\n")

    builder.toString()

  def locationsWithManhattanDistanceFrom(start: Location, distance: Int): Seq[Location] =
    (for
      xDelta <- -distance to distance
      yDeltaAbs = distance - xDelta.abs
      yDelta <- Set(-yDeltaAbs, yDeltaAbs)
    yield start.move(xDelta, yDelta))
      .filter(isWithinBorders)

  def locationsWithinManhattanDistanceFrom(start: Location, maxDistance: Int, minDistance: Int = 1): Seq[Location] =
    (minDistance to maxDistance).flatMap(locationsWithManhattanDistanceFrom(start, _))

object FullGridXy:
  case class Location(x: Int, y: Int):
    def move(direction: Direction4): Location =
      direction match
        case Direction4.Up    => Location(x, y - 1)
        case Direction4.Down  => Location(x, y + 1)
        case Direction4.Left  => Location(x - 1, y)
        case Direction4.Right => Location(x + 1, y)

    def move(xDelta: Int, yDelta: Int): Location = this.copy(x = x + xDelta, y = y + yDelta)

    def allNeighbors4: Seq[Location] =
      Direction4.values.map(direction => move(direction))

    def isNeighbor4Of(other: Location): Boolean = allNeighbors4.contains(other)

    override def toString: String = s"[$x,$y]"

    infix def manhattanDistanceTo(other: Location): Long = (x - other.x).abs + (y - other.y).abs

    def direction4FromNeighbor(neighbor: Location): Direction4 =
      assert(neighbor.isNeighbor4Of(this))
      Direction4.values.find(neighbor.move(_) == this).get

    def direction8To(other: Location): Direction8 =
      (other.x - x, other.y - y) match
        case (0, deltaY) if deltaY < 0                    => Direction8.Up
        case (0, deltaY) if deltaY > 0                    => Direction8.Down
        case (deltaX, 0) if deltaX < 0                    => Direction8.Left
        case (deltaX, 0) if deltaX > 0                    => Direction8.Right
        case (deltaX, deltaY) if deltaX < 0 && deltaY < 0 => Direction8.UpLeft
        case (deltaX, deltaY) if deltaX > 0 && deltaY < 0 => Direction8.UpRight
        case (deltaX, deltaY) if deltaX < 0 && deltaY > 0 => Direction8.DownLeft
        case (deltaX, deltaY) if deltaX > 0 && deltaY > 0 => Direction8.DownRight

    def verticalDistanceTo(other: Location): Int = (y - other.y).abs

    def horizontalDistanceTo(other: Location): Int = (x - other.x).abs

  case class LocationWithValue[T](location: Location, value: T):
    def show(using Show[T]): Unit = print(s"($location -> ${value.show})")
