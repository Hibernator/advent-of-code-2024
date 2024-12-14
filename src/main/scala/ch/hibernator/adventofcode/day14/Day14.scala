package ch.hibernator.adventofcode.day14

import ch.hibernator.adventofcode.SolutionBaseSimple
import ch.hibernator.adventofcode.util.{CoordinatesXy, VectorXy}

import java.io.{File, FileWriter}
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object Day14 extends SolutionBaseSimple:
  override def day: Int = 14

  override def solve(input: Seq[String]): (Long, Long) =
    val originalRobots = input.zipWithIndex.map { (line, index) =>
      val (rawPosition, rawVelocity) = line.split(" ").pipe { characteristics =>
        (characteristics.head, characteristics.last)
      }
      val position = rawPosition
        .substring(2)
        .split(",")
        .pipe(coordinates => CoordinatesXy(coordinates.head.toLong, coordinates.last.toLong))
      val velocity =
        rawVelocity.substring(2).split(",").pipe(velocity => VectorXy(velocity.head.toLong, velocity.last.toLong))
      Robot(index, position, velocity)
    }

    val (width, height) = if originalRobots.size == 12 then (11, 7) else (101, 103)

    val positionToRobots: mutable.LinkedHashMap[CoordinatesXy, mutable.LinkedHashSet[Robot]] = mutable.LinkedHashMap()

    def addRobotToMap(robot: Robot): Unit =
      positionToRobots.get(robot.position) match {
        case Some(robots) => robots.add(robot)
        case None         => positionToRobots.addOne(robot.position, mutable.LinkedHashSet(robot))
      }

    originalRobots.foreach(addRobotToMap)

    def removeRobotFromMap(robot: Robot): Unit =
      val robotsInPosition = positionToRobots(robot.position)
      if robotsInPosition.size > 1 then robotsInPosition.remove(robot) else positionToRobots.remove(robot.position)

    def moveRobot(robot: Robot, steps: Long): Robot =
      val rawNewX = (robot.position.x + robot.velocity.xChange * steps) % width
      val newX = if rawNewX < 0 then width + rawNewX else rawNewX
      val rawNewY = (robot.position.y + robot.velocity.yChange * steps) % height
      val newY = if rawNewY < 0 then height + rawNewY else rawNewY
      removeRobotFromMap(robot)
      val newRobot = Robot(robot.id, CoordinatesXy(newX, newY), robot.velocity)
      addRobotToMap(newRobot)
      newRobot

    val movedRobots = originalRobots.map(moveRobot(_, 100))

    val quadrants = Seq(
      Quadrant(CoordinatesXy(0, 0), CoordinatesXy(width / 2 - 1, height / 2 - 1)),
      Quadrant(CoordinatesXy(width / 2 + 1, 0), CoordinatesXy(width - 1, height / 2 - 1)),
      Quadrant(CoordinatesXy(0, height / 2 + 1), CoordinatesXy(width / 2 - 1, height - 1)),
      Quadrant(CoordinatesXy(width / 2 + 1, height / 2 + 1), CoordinatesXy(width - 1, height - 1))
    )

    def isRobotInQuadrant(robot: Robot, quadrant: Quadrant): Boolean =
      robot.position.x >= quadrant.topLeft.x && robot.position.x <= quadrant.bottomRight.x &&
        robot.position.y >= quadrant.topLeft.y && robot.position.y <= quadrant.bottomRight.y

    val quadrantToRobot: mutable.Map[Quadrant, mutable.Buffer[Robot]] = mutable.LinkedHashMap()
    for quadrant <- quadrants
    do quadrantToRobot.addOne(quadrant, mutable.Buffer())

    for
      quadrant <- quadrants
      robot <- movedRobots
    do if isRobotInQuadrant(robot, quadrant) then quadrantToRobot(quadrant).append(robot)

    val result1 = quadrantToRobot.values.map(_.size).product
    println(result1)

    def printGrid(): Unit =
      println("Board\n\n")
      for
        y <- 0 until height
        x <- 0 until width
      do
        val toPrint = if positionToRobots.get(CoordinatesXy(x, y)).exists(_.nonEmpty) then "R" else "."
        if x == width - 1 then println(toPrint) else print(toPrint)
      println("\n\n")

    def printGridToFile(writer: FileWriter): Unit =
      writer.append("Board\n\n")
      for
        y <- 0 until height
        x <- 0 until width
      do
        val toPrint = if positionToRobots.get(CoordinatesXy(x, y)).exists(_.nonEmpty) then "R" else "."
        if x == width - 1 then writer.append(s"$toPrint\n") else writer.append(toPrint)
      writer.append("\n\n")

    // Second part
    // Print a grid to file 1 to 100 000 steps
    // Search the file for the RRRRRRRRR pattern (works well in less)
    if originalRobots.size > 20 then
      val gridFile = new File("./src/main/resources/robots.txt")
      val writer = new FileWriter(gridFile)
      println(gridFile.getName)
      for steps <- 1 to 100_000
      do
        positionToRobots.clear()
        originalRobots.foreach(addRobotToMap)
        originalRobots.map(moveRobot(_, steps))
        writer.write(s"Steps: $steps\n")
        printGridToFile(writer)
      writer.close()

    (result1, 1L)

case class Robot(id: Int, position: CoordinatesXy, velocity: VectorXy)
case class Quadrant(topLeft: CoordinatesXy, bottomRight: CoordinatesXy)
