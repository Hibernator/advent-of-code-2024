package ch.hibernator.adventofcode.day7

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day7.Operator.*

import scala.annotation.targetName
import scala.collection.immutable.ListSet

object Day7 extends SolutionBaseSimple:
  override def day: Int = 7

  override def solve(input: Seq[String]): (Long, Long) =
    val equations = input.foldLeft(ListSet[Equation]()) { case (acc, line) =>
      val parts = line.split(": ")
      acc + Equation(parts.head.toLong, parts.last.split(" ").map(_.toLong))
    }

    val equationsWith2OperatorCombinations = equations.foldLeft(Seq[(Equation, Array[Array[Operator]])]()) {
      case (acc, equation) =>
        val operatorCombinations = twoOperatorCombinationsForEquation(equation, +, *)
        acc :+ (equation, operatorCombinations)
    }

    val correct2OperatorEquations = equationsWith2OperatorCombinations.filter { case (equation, operatorCombinations) =>
      operatorCombinations.exists(isOperatorSequenceCorrect(equation, _))
    }

    val equationsWith3OperatorCombinations = equations.foldLeft(Seq[(Equation, Array[Array[Operator]])]()) {
      case (acc, equation) =>
        val operatorCombinations = threeOperatorCombinationsForEquation(equation, +, *, ||)
        acc :+ (equation, operatorCombinations)
    }

    val correct3OperatorEquations = equationsWith3OperatorCombinations.filter { case (equation, operatorCombinations) =>
      operatorCombinations.exists(isOperatorSequenceCorrect(equation, _))
    }

    (correct2OperatorEquations.map(_._1.result).sum, correct3OperatorEquations.map(_._1.result).sum)

  private def twoOperatorCombinationsForEquation(
      equation: Equation,
      operator1: Operator,
      operator2: Operator
  ): Array[Array[Operator]] =
    val numOperations = equation.numbers.size - 1
    val initialUnexpandedCombinations = Array.fill(numOperations + 1, numOperations)(Operator.+)
    for
      i <- initialUnexpandedCombinations.indices
      j <- i until numOperations
    do initialUnexpandedCombinations(i)(j) = operator2

    initialUnexpandedCombinations.flatMap(combination => combination.permutations)

  // this can be generalized
  private def threeOperatorCombinationsForEquation(
      equation: Equation,
      operator1: Operator,
      operator2: Operator,
      operator3: Operator
  ): Array[Array[Operator]] =
    val numOperations = equation.numbers.size - 1
    val operatorAmounts =
      (for
        i <- 0 to numOperations
        j <- 0 to numOperations
        k <- 0 to numOperations
      yield (i, j, k)).filter((i, j, k) => i + j + k == numOperations)

    val unexpandedCombinations =
      for (op1Amount, op2Amount, op3Amount) <- operatorAmounts
      yield
        val unexpandedCombination = Array.fill(numOperations)(Operator.fake)
        for index <- 0 until op1Amount
        do unexpandedCombination(index) = operator1
        for index <- op1Amount until op1Amount + op2Amount
        do unexpandedCombination(index) = operator2
        for index <- op1Amount + op2Amount until numOperations
        do unexpandedCombination(index) = operator3
        unexpandedCombination

    unexpandedCombinations.flatMap(_.permutations).toArray

  private def applyOperatorsToEquation(equation: Equation, operators: Array[Operator]): Long =
    (equation.numbers.tail zip operators).foldLeft(equation.numbers.head) { case (acc, (number, op)) =>
      op.combine(acc, number)
    }

  def isOperatorSequenceCorrect(equation: Equation, operators: Array[Operator]): Boolean =
    applyOperatorsToEquation(equation, operators) == equation.result

end Day7

enum Operator:
  @targetName("plus") case +
  @targetName("times") case *
  @targetName("concatenate") case ||
  @targetName("fake") case fake

  def combine(operand1: Long, operand2: Long): Long =
    this match
      case +    => operand1 + operand2
      case *    => operand1 * operand2
      case ||   => s"$operand1$operand2".toLong
      case fake => sys.error("This operator can't be used")

case class Equation(result: Long, numbers: Seq[Long])
