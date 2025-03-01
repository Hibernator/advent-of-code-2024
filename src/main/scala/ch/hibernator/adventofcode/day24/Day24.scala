package ch.hibernator.adventofcode.day24

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day24.Operation.{AND, OR, XOR}

import java.lang
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day24 extends SolutionBaseSimple:
  override def day: Int = 24

  override def solve(input: Seq[String]): (Long, Long) =
    val splitIndex = input.indexWhere(_.isBlank)
    val (rawStartWires, rawGates) = (input.take(splitIndex), input.drop(splitIndex + 1))

    val wireNameToWire = mutable.Map[String, Wire]()

    rawGates.foreach: rawGate =>
      val components = rawGate.split(" ")
      val inWire1Name = components.head
      val operation = Operation.valueOf(components(1))
      val inWire2Name = components(2)
      val outWireName = components(4)

      val outWire = wireNameToWire.getOrElseUpdate(outWireName, Wire(outWireName))
      val gate = Gate(outWire, operation)
      val inWire1 = wireNameToWire.getOrElseUpdate(inWire1Name, Wire(inWire1Name))
      inWire1.addOutGate(gate)
      val inWire2 = wireNameToWire.getOrElseUpdate(inWire2Name, Wire(inWire2Name))
      inWire2.addOutGate(gate)

    rawStartWires.foreach: rawWire =>
      val wireName = rawWire.take(3)
      val wireValue = rawWire.last match
        case '0' => false
        case '1' => true
        case _   => sys.error("Unexpected wire value")

      wireNameToWire(wireName).setValue(wireValue)

    val result1 = wireNameToWire.view
      .filterKeys(_.head == 'z')
      .values
      .toSeq
      .sortBy(_.name)
      .map(_.getValue)
      .map:
        case false => "0"
        case true  => "1"
      .reverse
      .mkString
      .pipe(lang.Long.parseLong(_, 2))

    (result1, 1L)

enum Operation:
  case AND
  case OR
  case XOR

case class Wire(name: String):
  private var value: Option[Boolean] = None
  private val outGates = mutable.Buffer[Gate]()

  def setValue(newValue: Boolean): Unit =
    value = Some(newValue)
    outGates.foreach(_.addInput(newValue))

  def getValue: Boolean = value.get

  def addOutGate(gate: Gate): Unit = outGates.append(gate)

  override def toString: String = s"[$name: ${value.getOrElse("X")}]"

case class Gate(out: Wire, operation: Operation):
  private val inputs = mutable.Buffer[Boolean]()
  def addInput(input: Boolean): Unit =
    inputs.addOne(input)
    if inputs.size == 2 then combineInputs()

  private def combineInputs(): Unit =
    val (in1, in2) = (inputs.head, inputs.last)
    val outValue = operation match
      case AND => in1 && in2
      case OR  => in1 || in2
      case XOR => in1 != in2
    out.setValue(outValue)
