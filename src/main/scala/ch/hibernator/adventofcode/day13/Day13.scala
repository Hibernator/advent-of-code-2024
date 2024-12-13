package ch.hibernator.adventofcode.day13

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.{CoordinatesXy, MathUtil, VectorXy}

object Day13 extends SolutionBaseSimple:
  override def day: Int = 13

  override def solve(input: Seq[String]): (Long, Long) =
    val machines = input
      .grouped(4)
      .map { rawMachine =>
        val rawButtonA = rawMachine.head.split(" ")
        val vectorButtonA = VectorXy(rawButtonA(2).substring(2, 4).toInt, rawButtonA(3).substring(2).toInt)
        val rawButtonB = rawMachine(1).split(" ")
        val vectorButtonB = VectorXy(rawButtonB(2).substring(2, 4).toInt, rawButtonB(3).substring(2).toInt)
        val rawPrize = rawMachine(2).split(" ")
        val prizeLocation = CoordinatesXy(rawPrize(1).substring(2).init.toInt, rawPrize(2).substring(2).toInt)
        ClawMachine(vectorButtonA, vectorButtonB, prizeLocation)
      }
      .toSeq

    val result1Naive = machines.map(_.calculatePriceNaive).sum
    val result1Fast = machines.map(_.calculatePriceFast).sum
    assert(result1Naive == result1Fast)

    val newMachines = machines.map(machine =>
      machine.copy(prizeLocation =
        CoordinatesXy(machine.prizeLocation.x + 10000000000000L, machine.prizeLocation.y + 10000000000000L)
      )
    )
    val result2 = newMachines.map(_.calculatePriceFast).sum
    (result1Fast, result2)

case class ClawMachine(
    vectorButtonA: VectorXy,
    vectorButtonB: VectorXy,
    prizeLocation: CoordinatesXy
):
  override def toString: String = s"BtnA:$vectorButtonA. BtnB:$vectorButtonB. Prize:$prizeLocation"

  def calculatePriceNaive: Long =
    val minTotalPresses: Long =
      val maxVector =
        VectorXy(vectorButtonA.xChange.max(vectorButtonB.xChange), vectorButtonA.yChange.max(vectorButtonB.yChange))
      (prizeLocation.x / maxVector.xChange - 1).max(prizeLocation.y / maxVector.yChange - 1)

    val maxTotalPresses: Long =
      val minVector =
        VectorXy(vectorButtonA.xChange.min(vectorButtonB.xChange), vectorButtonA.yChange.min(vectorButtonB.yChange))
      (prizeLocation.x / minVector.xChange + 1).min(prizeLocation.y / minVector.yChange + 1).min(200)

    val minButtonCombinations =
      for pressesButtonA <- 0L to minTotalPresses
      yield (pressesButtonA, minTotalPresses - pressesButtonA)

    val allButtonCombinations =
      (for
        minButtonCombination <- minButtonCombinations
        pressesButtonA <- minButtonCombination._1 to 100
        pressesButtonB <- minButtonCombination._2 to 100
      yield (pressesButtonA, pressesButtonB)).filter((a, b) => a + b <= maxTotalPresses).distinct

    val winningCombinations = allButtonCombinations.filter { (aPresses, bPresses) =>
      aPresses * vectorButtonA.xChange + bPresses * vectorButtonB.xChange == prizeLocation.x &&
      aPresses * vectorButtonA.yChange + bPresses * vectorButtonB.yChange == prizeLocation.y
    }

    if winningCombinations.nonEmpty then winningCombinations.map { (a, b) => a * 3 + b }.min else 0

  private def winningCombination: Option[(Long, Long)] =
    val equationX = Equation(vectorButtonA.xChange, vectorButtonB.xChange, prizeLocation.x)
    val equationY = Equation(vectorButtonA.yChange, vectorButtonB.yChange, prizeLocation.y)

    // solve for b presses
    val aLcm = MathUtil.lcm(equationX.aMultiple, equationY.aMultiple)

    val aMultiplierEquationX = aLcm / equationX.aMultiple
    val equationXMultipliedByMultiplierA = equationX.multiply(aMultiplierEquationX)
    val aMultiplierEquationY = aLcm / equationY.aMultiple
    val equationYMultipliedByMultiplierA = equationY.multiply(aMultiplierEquationY)

    val subtractedEquationWithoutA = equationXMultipliedByMultiplierA.subtract(equationYMultipliedByMultiplierA)
    val bPresses = subtractedEquationWithoutA.solveB.getOrElse(sys.error("Should be solvable for B now"))

    // solve for a presses
    val bLcm = MathUtil.lcm(equationX.bMultiple, equationY.bMultiple)

    val bMultiplierEquationX = bLcm / equationX.bMultiple
    val equationXMultipliedByMultiplierB = equationX.multiply(bMultiplierEquationX)
    val bMultiplierEquationY = bLcm / equationY.bMultiple
    val equationYMultipliedByMultiplierB = equationY.multiply(bMultiplierEquationY)

    val subtractedEquationWithoutB = equationXMultipliedByMultiplierB.subtract(equationYMultipliedByMultiplierB)
    val aPresses = subtractedEquationWithoutB.solveA.getOrElse(sys.error("Should be solvable for A now"))

    Option.when(aPresses.isWhole && bPresses.isWhole)((aPresses.toLong, bPresses.toLong))

  def calculatePriceFast: Long = winningCombination.map((a, b) => a * 3 + b).getOrElse(0L)

case class Equation(aMultiple: BigDecimal, bMultiple: BigDecimal, result: BigDecimal):
  def multiply(multiplier: BigDecimal): Equation =
    Equation(aMultiple * multiplier, bMultiple * multiplier, result * multiplier)

  def subtract(anotherEquation: Equation): Equation = Equation(
    aMultiple - anotherEquation.aMultiple,
    bMultiple - anotherEquation.bMultiple,
    result - anotherEquation.result
  )

  def solveA: Option[BigDecimal] = Option.when(bMultiple == MathUtil.zero)(result / aMultiple)
  def solveB: Option[BigDecimal] = Option.when(aMultiple == MathUtil.zero)(result / bMultiple)
