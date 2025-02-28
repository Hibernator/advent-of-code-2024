package ch.hibernator.adventofcode.day23

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.mutable.SimpleGraph

import scala.collection.mutable

object Day23 extends SolutionBaseSimple:
  override def day: Int = 23

  override def solve(input: Seq[String]): (Long, Long) =
    val graph = new SimpleGraph[String]

    val connections = input.map(raw => (raw.take(2), raw.drop(3)))
    connections.foreach: connection =>
      val (vertex1, vertex2) = connection
      graph.addVertex(vertex1)
      graph.addVertex(vertex2)
      graph.addNeighbor(vertex1, vertex2)
      graph.addNeighbor(vertex2, vertex1)

    val setsOf3 = mutable.Set[Set[String]]()
    graph.allVertices
      .filter(_.head == 't')
      .foreach: vertex =>
        val neighbors = graph.neighborsOfVertex(vertex)
        val neighborsCombinationsOf2 = neighbors.toSeq.combinations(2)

        neighborsCombinationsOf2
          .filter: neighbors =>
            graph.neighborsOfVertex(neighbors.head).contains(neighbors.last)
          .foreach: neighbors =>
            setsOf3.addOne(Set(vertex, neighbors.head, neighbors.last))

    val result1 = setsOf3.size

    // Part 2
    /*
      Each vertex in the test input has exactly 4 neighbors, in the real input 13 neighbors.
      Therefore, the maximum possible size of interconnected set is 5 and 14.
      Turns out, it's 4 and 13 (test and real input).

      Thanks to the input structure, the solution doesn't require any graph algorithms and is still very fast
      with set operations only.

      In graph terms, we're looking for the largest clique. Bron-Kerbosch algorithm can be used for this.
     */

    // Finds all interconnected sets of size n
    def setsOfN(n: Int): Set[Set[String]] =
      val sets = mutable.Set[Set[String]]()
      graph.allVertices
        .foreach: vertex =>
          val neighbors = graph.neighborsOfVertex(vertex)
          val neighborsCombinations = neighbors.subsets(n - 1)

          neighborsCombinations
            .filter: neighbors =>
              neighbors.forall: neighbor =>
                neighbors.excl(neighbor).subsetOf(graph.neighborsOfVertex(neighbor))
            .foreach: neighbors =>
              sets.addOne(neighbors.incl(vertex))
      sets.toSet

    val largeSets = setsOfN(13)
    val largeSet = largeSets.headOption
    val result2 = largeSet.map(_.toSeq.sorted.mkString(","))
    println(result2)

    (result1, 1L)
