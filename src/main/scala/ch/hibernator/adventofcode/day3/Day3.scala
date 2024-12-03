package ch.hibernator.adventofcode.day3

import ch.hibernator.adventofcode.SolutionBase

import scala.util.chaining.scalaUtilChainingOps

object Day3 extends SolutionBase[String]:
  private val multiplierRegex = """mul\((\d{1,3}),(\d{1,3})\)""".r

  override def day: Int = 3

  override def calculateCommonResult(input: Seq[String]): String = input.head

  override def solvePart1(input: Seq[String], commonResult: String): Long =
    findAndAddMultiplications(commonResult)

  override def solvePart2(input: Seq[String], commonResult: String): Long =
    val (first, rest) = commonResult.split("don't\\(\\)").pipe(allSections => allSections.head -> allSections.tail)
    val enabled = (first +: rest.filter(_.contains("do()")).map(_.split("do\\(\\)").tail.mkString)).mkString
    findAndAddMultiplications(enabled)

  /** Finds multiplication instructions, executes them and sums them up
    * @param input
    *   computer memory
    * @return
    *   summed up multiplications
    */
  private def findAndAddMultiplications(input: String): Long =
    multiplierRegex
      .findAllMatchIn(input)
      .map(aMatch => aMatch.group(1).toLong * aMatch.group(2).toLong)
      .sum
