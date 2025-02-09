package ch.hibernator.adventofcode.day19

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec
import scala.collection.mutable

object Day19 extends SolutionBaseSimple:
  override def day: Int = 19

  override def solve(input: Seq[String]): (Long, Long) =
    val allPatterns = input.head.split(", ").sortBy(_.length).reverse
    val designs = input.drop(2)

    // The method finds one arrangement but is unsuitable for finding all of them
    // because both runtime and memory complexity is too high
    @tailrec
    def createDesign(stateStack: mutable.Stack[State], deadEnds: mutable.Set[String]): Seq[String] =
      // nothing else to try, impossible to create the design
      if stateStack.isEmpty then Nil
      else
        val currentState = stateStack.pop()
        // design created successfully
        if currentState.remainingDesign.isEmpty then currentState.usedPatterns
        // dead end encountered before, can backtrack right away
        else if deadEnds.contains(currentState.remainingDesign) then createDesign(stateStack, deadEnds)
        // new dead end, add to the list of know dead-ends and backtrack
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

    val result1 = designs
      .map(design => createDesign(mutable.Stack(State(design, allPatterns, Seq[String]())), mutable.Set()))
      .count(_.nonEmpty)

    // Finds number of all arrangements but not the actual arrangements
    def findNumAllArrangements(design: String): Long =
      val designLength = design.length
      // key is the length from the end of the design
      // value is the number of arrangements for sub-design
      val designToNumArrangements = mutable.Map[Int, Long]().withDefaultValue(0L)

      // iterate through all sub-designs from end
      for i <- 1 to design.length
      do
        val toDrop = designLength - i
        val subDesign = design.drop(toDrop)
        val matchingPatterns = allPatterns.filter(subDesign.startsWith)
        val numAllNewArrangements = matchingPatterns.map { pattern =>
          if pattern.length == i then 1 // the pattern is at the end of arrangement
          else designToNumArrangements(i - pattern.length)
        }.sum
        designToNumArrangements.addOne(i, numAllNewArrangements)

      designToNumArrangements(designLength)

    val result2 = designs.map(findNumAllArrangements).sum

    (result1, result2)

case class State(remainingDesign: String, availablePatterns: Seq[String], usedPatterns: Seq[String])
