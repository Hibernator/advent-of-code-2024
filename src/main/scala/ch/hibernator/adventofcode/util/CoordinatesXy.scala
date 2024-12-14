package ch.hibernator.adventofcode.util

case class CoordinatesXy(x: Long, y: Long):
  def move(direction: Direction4): CoordinatesXy =
    direction match
      case Direction4.Up    => CoordinatesXy(x, y - 1)
      case Direction4.Down  => CoordinatesXy(x, y + 1)
      case Direction4.Left  => CoordinatesXy(x - 1, y)
      case Direction4.Right => CoordinatesXy(x + 1, y)

  def allNeighbors4: Seq[CoordinatesXy] =
    Direction4.values.map(direction => move(direction))

  override def toString: String = s"[$x,$y]"
