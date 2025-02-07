package ch.hibernator.adventofcode.day19

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec
import scala.collection.mutable

object Day19 extends SolutionBaseSimple:
  override def day: Int = 19

  override def solve(input: Seq[String]): (Long, Long) =
    val allPatterns = input.head.split(", ").sortBy(_.length).reverse
    val designs = input.drop(2)

    @tailrec
    def createDesign(stateStack: mutable.Stack[State], deadEnds: mutable.Set[String]): Seq[String] =
      // nothing else to try, impossible to create the design
      if stateStack.isEmpty then Nil
      else
        val currentState = stateStack.pop()
        // design created successfully
        if currentState.remainingDesign.isEmpty then currentState.usedPatterns
        // dead end, can backtrack right away
        else if deadEnds.contains(currentState.remainingDesign) then createDesign(stateStack, deadEnds)
        // dead end, have to backtrack
        else if currentState.availablePatterns.isEmpty then
          createDesign(stateStack, deadEnds.addOne(currentState.remainingDesign))
        else
          val patternToTry = currentState.availablePatterns.head
          // The next available pattern fits
          if currentState.remainingDesign.startsWith(patternToTry) then
            // state to potentially come back to
            val stateForBacktracking = currentState.copy(availablePatterns = currentState.availablePatterns.tail)
            stateStack.push(stateForBacktracking)
            // state to process in the next step
            val nextState =
              currentState.copy(
                remainingDesign = currentState.remainingDesign.drop(patternToTry.length),
                availablePatterns = allPatterns,
                usedPatterns = currentState.usedPatterns :+ patternToTry
              )
            stateStack.push(nextState)
            createDesign(stateStack, deadEnds)
          // the pattern didn't fit. Remove the pattern from the state and try the next pattern
          else
            stateStack.push(currentState.copy(availablePatterns = currentState.availablePatterns.tail))
            createDesign(stateStack, deadEnds)

    val result1 =
      designs
        .map { design =>
          println(s"Processing design $design")
          val patterns = createDesign(mutable.Stack(State(design, allPatterns, Seq[String]())), mutable.Set())
          println(s"Patterns: ${patterns.mkString(",")}")
          patterns
        }
        .count(_.nonEmpty)

    println(result1)

    (result1, 1L)

case class State(remainingDesign: String, availablePatterns: Seq[String], usedPatterns: Seq[String])
