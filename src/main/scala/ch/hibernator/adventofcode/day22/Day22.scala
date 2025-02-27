package ch.hibernator.adventofcode.day22

import ch.hibernator.adventofcode.SolutionBaseSimple

import java.lang
import scala.annotation.tailrec
import scala.collection.mutable

object Day22 extends SolutionBaseSimple:
  override def day: Int = 22

  override def solve(input: Seq[String]): (Long, Long) =
    val initialPrices = input.map(_.toInt)

    // Since all the numbers involved in calculations are powers of 2, bitwise operations can be used for efficiency
    def generateNextPrice(currentPrice: Long): Long =
      val step1 = ((currentPrice << 6) ^ currentPrice) & 16777215
      val step2 = ((step1 >> 5) ^ step1) & 16777215
      ((step2 << 11) ^ step2) & 16777215

    def nthPrice(initialPrice: Long, n: Int): Long =
      @tailrec
      def acc(currentPrice: Long, counter: Int): Long =
        if counter == n then currentPrice else acc(generateNextPrice(currentPrice), counter + 1)

      acc(initialPrice, 0)

    val result1 = initialPrices.map(nthPrice(_, 2000)).sum

    // Part 2
    def nPrices(initialPrice: Long, n: Int): Seq[Long] =
      @tailrec
      def acc(accPrices: Seq[Long], counter: Int): Seq[Long] =
        if counter == n then accPrices else acc(accPrices :+ generateNextPrice(accPrices.last), counter + 1)

      acc(Seq(initialPrice), 0)

//    val pricesOfBuyers: Seq[Seq[Byte]] = initialPrices.map(nPrices(_, 2000))

    // Prices per buyer and sequence
    val buyerAndSeqToPrice = mutable.Map[(Int, Seq[Byte]), Byte]()

    def longPriceToOneDigit(longPrice: Long): Byte = longPrice.toString.last.asDigit.toByte

    // Populate the map
    // If a sequence is encountered more than once for a buyer, only the first one counts
    // Because the second one would never be reached
    for buyerIndex <- initialPrices.indices
    do
      val first5prices = nPrices(initialPrices(buyerIndex), 4)
      val first5ActualPrices = first5prices.map(longPriceToOneDigit)
      val firstSequence = first5ActualPrices.init.zip(first5prices.tail).map((first, second) => (second - first).toByte)

      @tailrec
      def processSequence(sequence: Seq[Byte], price: Long, generatedPriceIndex: Int): Unit =
        val oneDigitPrice = longPriceToOneDigit(price)
        if !buyerAndSeqToPrice.contains((buyerIndex, sequence)) then
          buyerAndSeqToPrice.update((buyerIndex, sequence), oneDigitPrice)
        if generatedPriceIndex == 2000 then ()
        else
          val nextPrice = generateNextPrice(price)
          val nextOneDigitPrice = longPriceToOneDigit(nextPrice)
          val newSequence = sequence.tail :+ (nextOneDigitPrice - oneDigitPrice).toByte
          processSequence(newSequence, nextPrice, generatedPriceIndex + 1)

      processSequence(firstSequence, first5prices.last, 4)

    def totalPriceForSequence(sequence: Seq[Byte]): Int =
      input.indices.map(buyerIndex => buyerAndSeqToPrice.getOrElse((buyerIndex, sequence), 0.toByte)).map(_.toInt).sum

    /*
      Not the most efficient solution but still fast enough
      Potential improvements
      - parallelize calculating total prices for sequences
      - check only sequences leading to high prices for at least one buyer
     */
    val result2 = buyerAndSeqToPrice.keySet.map(_._2).toSeq.map(totalPriceForSequence).max

    (result1, result2)
