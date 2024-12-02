package ch.hibernator.adventofcode.day2

import ch.hibernator.adventofcode.SolutionBase

object Day2 extends SolutionBase[Seq[Seq[Int]]]:
  override def day: Int = 2

  override def calculateCommonResult(input: Seq[String]): Seq[Seq[Int]] = {
    input.map(_.split(" ").map(_.toInt).toSeq)
  }

  override def solvePart1(input: Seq[String], reports: Seq[Seq[Int]]): Long = {
    reports.count(isSafeNoTolerance)
  }

  override def solvePart2(input: Seq[String], reports: Seq[Seq[Int]]): Long =
    reports.count(isSafeWithTolerance)

  private def isSafeNoTolerance(report: Seq[Int]): Boolean = {
    val pairs = report.init.zip(report.tail)
    val diffs = pairs.map(_ - _)
    val allDiffsInRange = diffs.forall { diff =>
      val diffAbs = diff.abs
      diffAbs >= 1 && diffAbs <= 3
    }
    val monotone = diffs.forall(_ > 0) || diffs.forall(_ < 0)
    allDiffsInRange && monotone
  }

  // not the most efficient but works well enough on this input
  private def isSafeWithTolerance(report: Seq[Int]): Boolean = {
    val reportsWithElementRemoved = for {
      split <- 1 to report.size
    } yield report.take(split - 1) ++ report.drop(split)
    isSafeNoTolerance(report) || reportsWithElementRemoved.exists(isSafeNoTolerance)
  }
