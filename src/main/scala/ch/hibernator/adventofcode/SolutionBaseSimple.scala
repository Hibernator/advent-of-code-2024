package ch.hibernator.adventofcode

import scala.io.Source
import scala.util.Using

trait SolutionBaseSimple:
  def day: Int
  def solve(input: Seq[String]): (Long, Long)

  private val inputFileAddress = s"./src/main/resources/input/$day.txt"
  protected val testInput: Seq[String] =
    Using(Source.fromFile(s"./src/main/resources/test-input/$day.txt"))(_.getLines().toSeq).get
  protected val input: Seq[String] =
    Using(Source.fromFile(inputFileAddress))(_.getLines().toSeq).get

  def main(args: Array[String]): Unit =
    val testResults = solve(testInput)
    val realResults = solve(input)
    println(s"Test result part 1: ${testResults._1}")
    println(s"Real result part 1: ${realResults._1}")
    println(s"Test result part 2: ${testResults._2}")
    println(s"Real result part 2: ${realResults._2}")
