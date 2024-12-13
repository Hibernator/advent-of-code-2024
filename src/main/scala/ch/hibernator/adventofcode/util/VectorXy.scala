package ch.hibernator.adventofcode.util

case class VectorXy(xChange: Long, yChange: Long):
  override def toString: String = s"[$xChange,$yChange]"
