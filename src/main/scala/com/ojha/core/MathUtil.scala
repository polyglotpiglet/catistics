package com.ojha.core

import scala.annotation.tailrec

object MathUtil {

  def factorial(n: Int): Int = {
    @tailrec
    def aux(n: Int, carry: Int): Int = n match {
      case 0 => carry
      case _ => aux(n-1, carry * n)
    }
    aux(n, 1)
  }

  def choose(n: Int, x: Int): Double = factorial(n) / (factorial(x) * factorial(n - x).toDouble)

  def round(dp: Int, double: Double): Double = {
    BigDecimal(double).setScale(dp, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
