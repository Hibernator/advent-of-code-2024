package ch.hibernator.adventofcode.util

case class CoordinatesXy(x: Long, y: Long):
  override def toString: String = s"[$x,$y]"
