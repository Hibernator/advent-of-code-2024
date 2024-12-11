package ch.hibernator.adventofcode.day11

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day11 extends SolutionBaseSimple:
  override def day: Int = 11

  override def solve(input: Seq[String]): (Long, Long) =
    val originalStones = input.head.split(" ").map(_.toLong).map(Stone.apply)

    def blinkMultipleTimes(numBlinks: Int): Long =
      @tailrec
      def blinkWithMap(stoneToAmount: mutable.Map[Stone, Long], blinksToDo: Int): Long =
        if blinksToDo == 0 then stoneToAmount.values.sum
        else
          val newStoneToAmount: mutable.Map[Stone, Long] = mutable.Map().withDefaultValue(0L)
          stoneToAmount.foreach { case (stone, amount) =>
            val newStones = stone.blink
            newStones.foreach { newStone =>
              newStoneToAmount.update(newStone, newStoneToAmount(newStone) + amount)
            }
          }
          blinkWithMap(newStoneToAmount, blinksToDo - 1)

      val stonesToAmount: mutable.Map[Stone, Long] = mutable.Map()
      originalStones.foreach(stone => stonesToAmount.addOne(stone, 1))
      blinkWithMap(stonesToAmount, numBlinks)

    val result1 = blinkMultipleTimes(25)
    val result2 = blinkMultipleTimes(75)

    (result1, result2)

case class Stone(id: Long):
  def blink: Seq[Stone] =
    if id == 0 then Seq(Stone(1))
    else
      val idStr = id.toString
      val idLength = idStr.length
      if idLength % 2 == 0 then
        idStr.splitAt(idLength / 2).pipe { case (id1, id2) =>
          Seq(Stone(id1.toLong), Stone(id2.toLong))
        }
      else Seq(Stone(id * 2024))
