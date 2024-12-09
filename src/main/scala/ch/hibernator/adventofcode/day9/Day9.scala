package ch.hibernator.adventofcode.day9

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.annotation.tailrec
import scala.collection.mutable

object Day9 extends SolutionBaseSimple:
  override def day: Int = 9

  override def solve(input: Seq[String]): (Long, Long) =
    val originalDisk = input.head.map(_.asDigit).zipWithIndex.foldLeft(Seq[Int]()) { case (diskAcc, (length, index)) =>
      if index % 2 == 0 then
        val fileId = index / 2
        diskAcc ++ Seq.fill(length)(fileId)
      else diskAcc ++ Seq.fill(length)(-1)
    }

    val compactedFragmentedDisk = mutable.Seq.from(originalDisk)

    @tailrec
    def fragmentStep(leftIndex: Int, rightIndex: Int): Unit =
      if leftIndex >= rightIndex then ()
      else
        (compactedFragmentedDisk(leftIndex), compactedFragmentedDisk(rightIndex)) match
          case (leftValue, rightValue) if leftValue != -1 && rightValue == -1 =>
            fragmentStep(leftIndex + 1, rightIndex - 1)
          case (leftValue, _) if leftValue != -1   => fragmentStep(leftIndex + 1, rightIndex)
          case (_, rightValue) if rightValue == -1 => fragmentStep(leftIndex, rightIndex - 1)
          case (leftValue, rightValue) =>
            compactedFragmentedDisk.update(leftIndex, rightValue)
            compactedFragmentedDisk.update(rightIndex, -1)
            fragmentStep(leftIndex + 1, rightIndex - 1)

    fragmentStep(0, compactedFragmentedDisk.size - 1)
    val result1 = compactedFragmentedDisk.takeWhile(_ > -1).map(_.toLong).zipWithIndex.map(_ * _).sum

    val compactedDisk = mutable.Seq.from(originalDisk)

    @tailrec
    def fileStep(leftIndex: Int, rightIndex: Int): Unit =

      @tailrec
      def findFileBeginning(fileEndIndex: Int, fileId: Int): Int =
        if compactedDisk(fileEndIndex - 1) == fileId then findFileBeginning(fileEndIndex - 1, fileId)
        else fileEndIndex

      @tailrec
      def findNewFileBeginning(searchStartIndex: Int, fileBeginningIndex: Int, fileLength: Int): Int =
        if searchStartIndex >= fileBeginningIndex then -1
        else if compactedDisk.slice(searchStartIndex, searchStartIndex + fileLength).forall(_ == -1) then
          searchStartIndex
        else findNewFileBeginning(searchStartIndex + 1, fileBeginningIndex, fileLength)

      def moveFile(fileBeginningIndex: Int, newFileBeginningIndex: Int, fileId: Int, fileLength: Int): Unit =
        for i <- newFileBeginningIndex until newFileBeginningIndex + fileLength
        do compactedDisk.update(i, fileId)
        for i <- fileBeginningIndex until fileBeginningIndex + fileLength
        do compactedDisk.update(i, -1)

      if leftIndex >= rightIndex then ()
      else
        (compactedDisk(leftIndex), compactedDisk(rightIndex)) match
          case (leftValue, rightValue) if leftValue != -1 && rightValue == -1 => fileStep(leftIndex + 1, rightIndex - 1)
          case (leftValue, _) if leftValue != -1                              => fileStep(leftIndex + 1, rightIndex)
          case (_, rightValue) if rightValue == -1                            => fileStep(leftIndex, rightIndex - 1)
          case (_, fileId) =>
            val fileEndIndex = rightIndex
            val fileBeginningIndex = findFileBeginning(fileEndIndex, fileId)
            val fileLength = fileEndIndex - fileBeginningIndex + 1
            val newFileBeginningIndex = findNewFileBeginning(leftIndex, fileBeginningIndex, fileLength)
            if newFileBeginningIndex == -1 then fileStep(leftIndex, rightIndex - fileLength)
            else
              moveFile(fileBeginningIndex, newFileBeginningIndex, fileId, fileLength)
              if newFileBeginningIndex == leftIndex then fileStep(leftIndex + fileLength, rightIndex - fileLength)
              else fileStep(leftIndex, rightIndex - fileLength)

    fileStep(0, compactedDisk.size - 1)
    val result2 = compactedDisk.zipWithIndex.filter(_._1 > -1).map(pair => (pair._1.toLong, pair._2)).map(_ * _).sum

    (result1, result2)
