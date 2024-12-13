package ch.hibernator.adventofcode.util

object InputOps:
  extension (input: Seq[String]) def numRowsAndColumns: (Int, Int) = (input.size, input.head.length)
