package ch.hibernator.adventofcode.day25

import ch.hibernator.adventofcode.SolutionBaseSimple

import scala.util.chaining.scalaUtilChainingOps

object Day25 extends SolutionBaseSimple:
  override def day: Int = 25

  override def solve(input: Seq[String]): (Long, Long) =
    val (locks, keys) = input
      .grouped(8)
      .map(group => if group.size == 8 then group.init else group)
      .map: raw =>
        raw.tail.init.transpose
          .map(_.count(_ == '#'))
          .pipe: pinsOrShears =>
            if raw.head.forall(_ == '#') then Left(Lock(pinsOrShears))
            else Right(Key(pinsOrShears))
      .foldLeft((Seq[Lock](), Seq[Key]())) { case ((locks, keys), lockOrKey) =>
        lockOrKey match
          case Left(lock) => (locks :+ lock, keys)
          case Right(key) => (locks, keys :+ key)
      }

    def isMatch(lock: Lock, key: Key): Boolean =
      lock.pins.zip(key.shears).forall(_ + _ <= 5)

    val matches = for
      lock <- locks
      key <- keys
    yield isMatch(lock, key)

    val result1 = matches.count(identity)

    (result1, 1L)

case class Key(shears: Seq[Int])
case class Lock(pins: Seq[Int])
