package ch.hibernator.adventofcode.util.mutable

import scala.collection.mutable

/** Bi-directional graph with distance of 1 between vertices.
  *
  * @tparam T
  *   type of vertex value
  */
class SimpleGraph[T]:
  private val vertexToNeighbors = mutable.Map[T, mutable.Set[T]]()

  def addVertex(vertex: T): Unit =
    if !vertexToNeighbors.contains(vertex) then vertexToNeighbors.update(vertex, mutable.Set())

  def addNeighbor(vertex: T, neighbor: T): Unit =
    if !vertexToNeighbors.contains(vertex) then vertexToNeighbors.update(vertex, mutable.Set(neighbor))
    else vertexToNeighbors(vertex).add(neighbor)

  def neighborsOfVertex(vertex: T): Set[T] = vertexToNeighbors(vertex).toSet

  def allVertices: Set[T] = vertexToNeighbors.keySet.toSet
