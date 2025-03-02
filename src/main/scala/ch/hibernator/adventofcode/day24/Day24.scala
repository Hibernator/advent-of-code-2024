package ch.hibernator.adventofcode.day24

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.day24.Operation.{AND, OR, XOR}

import java.lang
import scala.collection.mutable

object Day24 extends SolutionBaseSimple:
  override def day: Int = 24

  override def solve(input: Seq[String]): (Long, Long) =
    val splitIndex = input.indexWhere(_.isBlank)
    val (rawStartWires, rawGates) = (input.take(splitIndex), input.drop(splitIndex + 1))

    val wireNameToWire = mutable.Map[String, Wire]()

    // Create all gates
    rawGates.foreach: rawGate =>
      val components = rawGate.split(" ")
      val inWire1Name = components.head
      val operation = Operation.valueOf(components(1))
      val inWire2Name = components(2)
      val outWireName = components(4)

      val outWire = wireNameToWire.getOrElseUpdate(outWireName, Wire(outWireName))
      val gate = Gate(outWire, operation)
      outWire.inGate = Some(gate)
      val inWire1 = wireNameToWire.getOrElseUpdate(inWire1Name, Wire(inWire1Name))
      inWire1.addOutGate(gate)
      gate.addInWire(inWire1Name)
      val inWire2 = wireNameToWire.getOrElseUpdate(inWire2Name, Wire(inWire2Name))
      inWire2.addOutGate(gate)
      gate.addInWire(inWire2Name)

    // Read starting wires and set their values
    // This triggers the values propagation through the gates
    rawStartWires.foreach: rawWire =>
      val wireName = rawWire.take(3)
      val wireValue = rawWire.last match
        case '0' => false
        case '1' => true
        case _   => sys.error("Unexpected wire value")

      wireNameToWire(wireName).setValue(wireValue)

    def wiresToBinary(prefix: Char): String =
      wireNameToWire.view
        .filterKeys(_.head == prefix)
        .values
        .toSeq
        .sortBy(_.name)
        .map(_.getValue)
        .map:
          case false => "0"
          case true  => "1"
        .reverse
        .mkString

    val result1Binary = wiresToBinary('z')

    val result1Decimal = lang.Long.parseLong(result1Binary, 2)

    println(result1Decimal)

    // Part 2

    /*
      The goal of the system is to add two binary numbers which is implemented via a chain of full-adders
      (and half-adder for x00 and y00).
      The adders are implemented via AND, OR and XOR gates (not NAND or NOR).
      To find, which wires are swapped, several rules can be verified to ensure that all adders are created correctly.
     */

    // All z-wires should be coming out of a XOR gate except z45 (the last carry)
    val zWiresComingFromNonXorGate =
      wireNameToWire.view
        .filterKeys(_.head == 'z')
        .values
        .filter(_.inGate.exists(_.operation != XOR))
        .filterNot(_.name == "z45")
        .toSeq

    // Wires coming out of the second XOR must start with z
    val nonZWiresFromSecondXor = wireNameToWire.values
      .filterNot(_.name.startsWith("z"))
      .filter: wire =>
        wire.inGate.exists: gate =>
          gate.operation == XOR &&
            gate.inWireNames.forall: wireName =>
              !wireName.startsWith("x") && !wireName.startsWith("y")

    // All wires coming out of an AND gate should only go to an OR gate
    // Except for the half-adder case of x00 and y00
    val wiresComingFromAndNotLeadingToOr = wireNameToWire.values
      .filter(_.inGate.exists(_.operation == AND))
      .filterNot: wire =>
        wire.outGates.size == 1 && wire.outGates.head.operation == OR
      .filterNot: wire =>
        wire.inGate.exists: gate =>
          gate.operation == AND && gate.inWireNames.contains("x00") && gate.inWireNames.contains("y00")

    // Starting wires have to go to exactly two gates: XOR and AND
    val startingWiresNotGoingToXorAndAnd = wireNameToWire.view
      .filterKeys: name =>
        name.startsWith("x") || name.startsWith("y")
      .values
      .filterNot: wire =>
        wire.outGates.size == 2 &&
          wire.outGates.exists(_.operation == XOR) &&
          wire.outGates.exists(_.operation == AND)
      .toSeq

    // Wire coming from XOR that combines starting wires has to go to XOR and AND
    // Except for half-adder of the x00 and y00
    val wiresFromStartingXorNotGoingtoXorAndAnd = wireNameToWire.values
      .filter: wire =>
        wire.inGate.exists: gate =>
          gate.operation == XOR &&
            gate.inWireNames.exists(_.startsWith("x")) &&
            gate.inWireNames.exists(_.startsWith("y"))
      .filterNot: wire =>
        wire.name == "z00"
      .filterNot: wire =>
        wire.outGates.size == 2 &&
          wire.outGates.exists(_.operation == XOR) &&
          wire.outGates.exists(_.operation == AND)

    // Wire coming from XOR (carry out) must go to XOR and AND (as carry in)
    val carryNotGoingToXorAndAnd = wireNameToWire.values
      .filter: wire =>
        wire.inGate.exists(_.operation == OR)
      .filterNot(_.name == "z45")
      .filterNot: wire =>
        wire.outGates.size == 2 &&
          wire.outGates.exists(_.operation == XOR) &&
          wire.outGates.exists(_.operation == AND)
      .toSeq

    val result2 = (zWiresComingFromNonXorGate ++
      nonZWiresFromSecondXor ++
      wiresComingFromAndNotLeadingToOr ++
      startingWiresNotGoingToXorAndAnd ++
      wiresFromStartingXorNotGoingtoXorAndAnd ++
      carryNotGoingToXorAndAnd)
      .map(_.name)
      .distinct
      .sorted
      .mkString(",")
    println(result2)

    (result1Decimal, 1L)

enum Operation:
  case AND
  case OR
  case XOR

// Wire keeps track of in and out gates
// If the value (signal) is set, it's propagated to the gate
case class Wire(name: String, var inGate: Option[Gate] = None):
  private var value: Option[Boolean] = None
  val outGates: mutable.Buffer[Gate] = mutable.Buffer()

  def setValue(newValue: Boolean): Unit =
    value = Some(newValue)
    outGates.foreach(_.addInput(newValue))

  def getValue: Boolean = value.get

  def addOutGate(gate: Gate): Unit = outGates.append(gate)

  override def toString: String =
    s"Wire[name=$name; inGate=${inGate.getOrElse("None")}; outGates=${outGates.mkString(",")}; value=$valueText]"

  private def valueText = value match
    case None        => "X"
    case Some(true)  => "1"
    case Some(false) => "0"

// Gate keeps track of in and out wires
// When values from both input wires are received, the operation is performed and result sent to the output wire
case class Gate(out: Wire, operation: Operation):
  private val inputs = mutable.Buffer[Boolean]()
  var inWireNames: mutable.Buffer[String] = mutable.Buffer()
  val outWireName: String = out.name

  def addInput(input: Boolean): Unit =
    inputs.addOne(input)
    if inputs.size == 2 then combineInputs()

  def addInWire(name: String): Unit =
    inWireNames.append(name)
    inWireNames = inWireNames.sorted

  override def toString: String =
    s"Gate($operation, in1=${inWireNames.head}, in2=${inWireNames.last}, out=${outWireName})"

  private def combineInputs(): Unit =
    val (in1, in2) = (inputs.head, inputs.last)
    val outValue = operation match
      case AND => in1 && in2
      case OR  => in1 || in2
      case XOR => in1 != in2
    out.setValue(outValue)
