package ch.hibernator.adventofcode.day5

import ch.hibernator.adventofcode.SolutionBase

object Day5 extends SolutionBase[(Set[Seq[Int]], Set[Seq[Int]])]:
  override def day: Int = 5

  override def intermediateResult(input: Seq[String]): (Set[Seq[Int]], Set[Seq[Int]]) =
    val pageOrderPairs =
      input.takeWhile(_.nonEmpty).map(_.split("\\|")).map(pages => pages.head.toInt -> pages.last.toInt).toSet
    val pagesLists = input.drop(pageOrderPairs.size + 1).map(_.split(",").toSeq.map(_.toInt)).toSet

    given Ordering[Int] = Ordering.fromLessThan[Int]((x, y) => pageOrderPairs.contains(x -> y))

    val pageListsOrdered = pagesLists.map(_.sorted)
    pagesLists -> pageListsOrdered

  override def solvePart1(input: Seq[String], intermediateResult: (Set[Seq[Int]], Set[Seq[Int]])): Long =
    val (pageLists, pageListsOrdered) = intermediateResult
    val originallyCorrectlyOrdered = pageListsOrdered.intersect(pageLists)
    sumMiddleElements(originallyCorrectlyOrdered)

  override def solvePart2(input: Seq[String], intermediateResult: (Set[Seq[Int]], Set[Seq[Int]])): Long =
    val (pageLists, pageListsOrdered) = intermediateResult
    val newlyCorrectlyOrdered = pageListsOrdered.diff(pageLists)
    sumMiddleElements(newlyCorrectlyOrdered)

  private def sumMiddleElements(pageLists: Set[Seq[Int]]): Int =
    pageLists.foldLeft(0) { case (acc, pages) =>
      acc + pages((pages.size - 1) / 2)
    }
