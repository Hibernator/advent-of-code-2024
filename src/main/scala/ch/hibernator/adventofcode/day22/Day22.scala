package ch.hibernator.adventofcode.day22

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec

object Day22 extends SolutionBaseSimple:
  override def day: Int = 22

  override def solve(input: Seq[String]): (Long, Long) =
    val initialPrices = input.map(_.toInt)

    // Since all the numbers involved in calculations are powers of 2, bitwise operations can be used for efficiency
    def nextPrice(currentPrice: Long): Long =
      val step1 = ((currentPrice << 6) ^ currentPrice) & 16777215
      val step2 = ((step1 >> 5) ^ step1) & 16777215
      ((step2 << 11) ^ step2) & 16777215

    def nthPrice(initialPrice: Long, n: Int): Long =
      @tailrec
      def acc(currentPrice: Long, counter: Int): Long =
        if counter == n then currentPrice else acc(nextPrice(currentPrice), counter + 1)

      acc(initialPrice, 0)

    val result1 = initialPrices.map(nthPrice(_, 2000)).sum

    (result1, 1L)
