package ch.hibernator.adventofcode.day17

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.collection.mutable
import scala.math.BigInt.int2bigInt

object Day17 extends SolutionBaseSimple:
  override def day: Int = 17

  override def solve(input: Seq[String]): (Long, Long) =
    val computer = Computer()
    computer.setA(input.head.drop(12).toInt)
    computer.setB(input(1).drop(12).toInt)
    computer.setC(input(2).drop(12).toInt)

    val program = input.last.drop(9).split(",").map(_.toByte).toSeq
    val maxPointer = program.size - 1
    var instructionPointer: Int = 0
    val output: mutable.Buffer[Byte] = mutable.Buffer()
    var numSteps: Int = 0

    println(computer)
    println(s"Program: $program")

    def getComboOperandValue(pointer: Int): Long =
      program(pointer) match
        case op if op >= 0 && op <= 3 => op
        case 4                        => computer.getA
        case 5                        => computer.getB
        case 6                        => computer.getC
        case _                        => sys.error("Unsupported operand")

    def getLiteralOperandValue(pointer: Int): Byte = program(pointer)

    /** Executes a single instruction, using bitwise operations for efficiency, wherever possibler.</br> Moves the
      * pointer after the instruction is executed.
      * @param instruction
      *   instruction to execute
      */
    def executeInstruction(instruction: Instruction): Unit =
      instruction match
        case Instruction.adv =>
          computer.setA(computer.getA / (1 << getComboOperandValue(instructionPointer + 1)))
          instructionPointer += 2
        case Instruction.bxl =>
          computer.setB(computer.getB ^ getLiteralOperandValue(instructionPointer + 1))
          instructionPointer += 2
        case Instruction.bst =>
          computer.setB(getComboOperandValue(instructionPointer + 1) & 7)
          instructionPointer += 2
        case Instruction.jnz =>
          if computer.getA != 0 then instructionPointer = getLiteralOperandValue(instructionPointer + 1)
          else instructionPointer += 2
        case Instruction.bxc =>
          computer.setB(computer.getB ^ computer.getC)
          instructionPointer += 2
        case Instruction.out =>
          output.append((getComboOperandValue(instructionPointer + 1) & 7).toByte)
          instructionPointer += 2
        case Instruction.bdv =>
          computer.setB(computer.getA / (1 << getComboOperandValue(instructionPointer + 1)))
          instructionPointer += 2
        case Instruction.cdv =>
          computer.setC(computer.getA / (1 << getComboOperandValue(instructionPointer + 1)))
          instructionPointer += 2

    /** Performs one step: reads the next instruction and executes it
      */
    def performStep(): Unit =
      val instructionId = program(instructionPointer)
      val instruction = Instruction.fromOrdinal(instructionId)
      executeInstruction(instruction)
      numSteps += 1

    /** Runs the whole program
      */
    def runProgram(): Unit =
      while instructionPointer <= maxPointer do performStep()

    runProgram()

    println(output.mkString(","))
    println(s"Steps needed: $numSteps")

    // part 2

    /** Converts a string representing an octal number into a Long
      * @param octal
      *   octal number
      * @return
      *   decimal number (Long)
      */
    def octalToDecimal(octal: String): Long =
      val maxIndex = octal.length - 1
      octal.zipWithIndex.foldLeft(0L) { case (acc, (char, index)) =>
        (acc + char.asDigit * 8.pow(maxIndex - index)).toLong
      }

    // Solving the second part
    // - brute force doesn't work because numbers are too large
    // - Properties of the program to execute
    //   - first digit of octal representation of the initial value of the A registry determines the last output digit
    //   - second A digit determines the second to last output value and so on
    //   - the results remain stable as digits are added to initial A value
    //   - number of octal A digits equals output size
    // - Algorithm to get the lowest possible initial output
    //   - try all possible 1-digit inputs (there are 8 of them)
    //   - pick the lowest one of those that produce the correct last number of the output
    //   - try next 8 options by adding another octal digit and pick the lowest one again
    //   - when hitting a dead-end, backtrack and pick the second lowest input that produces correct output
    //   - it's essentially depth-first-search
    //   - search progression included in the day17search.txt file

    val initialInputs = (0 to 7).map(variable => s"$variable").map(octalToDecimal)

    initialInputs.foreach { in =>
      output.clear()
      computer.setA(in)
      computer.setB(0)
      computer.setC(0)
      instructionPointer = 0
      runProgram()
      println(s"Aoct = ${in.toOctalString}, output = ${output.mkString(",")}")
    }

    (1L, 1L)

end Day17

class Computer:
  private var registerA: Long = 0
  private var registerB: Long = 0
  private var registerC: Long = 0

  def setA(value: Long): Unit =
    registerA = value

  def setB(value: Long): Unit =
    registerB = value

  def setC(value: Long): Unit =
    registerC = value

  def getA: Long = registerA
  def getB: Long = registerB
  def getC: Long = registerC

  override def toString: String = s"Computer: A=$registerA, B=$registerB, C=$registerC"

enum Instruction:
  case adv
  case bxl
  case bst
  case jnz
  case bxc
  case out
  case bdv
  case cdv
