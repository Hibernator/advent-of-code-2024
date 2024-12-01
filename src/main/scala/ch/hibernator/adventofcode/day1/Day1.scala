package ch.hibernator.adventofcode.day1

import ch.hibernator.adventofcode.SolutionBase

object Day1 extends SolutionBase[(Seq[Int], Seq[Int])]:
  override def day: Int = 1

  override def calculateCommonResult(input: Seq[String]): (Seq[Int], Seq[Int]) =
    val (list1, list2) =
      input.map(_.split(" {3}")).map(both => (both.head.toInt, both.last.toInt)).foldLeft((Seq[Int](), Seq[Int]())) {
        case ((list1, list2), (first, second)) =>
          (list1 :+ first, list2 :+ second)
      }

    (list1.sorted, list2.sorted)

  override def solvePart1(input: Seq[String], commonResult: (Seq[Int], Seq[Int])): Long =
    val (sortedList1, sortedList2) = commonResult
    val distances = sortedList1.zip(sortedList2).map((item1, item2) => (item1 - item2).abs)
    distances.sum

  override def solvePart2(input: Seq[String], commonResult: (Seq[Int], Seq[Int])): Long =
    val (sortedList1, sortedList2) = commonResult
    val numOccurrences = sortedList1.map(item => sortedList2.count(_ == item))
    val similarityScore = sortedList1.zip(numOccurrences).map((item, occurrences) => item * occurrences).sum
    similarityScore
