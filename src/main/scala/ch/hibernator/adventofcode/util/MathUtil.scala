package ch.hibernator.adventofcode.util

import scala.annotation.tailrec

object MathUtil:
  val zero: BigDecimal = BigDecimal("0")

  @tailrec
  def gcd(a: BigDecimal, b: BigDecimal): BigDecimal =
    if a == 0 then b else gcd(b % a, a)

  def lcm(a: BigDecimal, b: BigDecimal): BigDecimal = (a / gcd(a, b)) * b
