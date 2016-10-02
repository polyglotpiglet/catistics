package com.ojha.core

import MathUtil._

object PoissonDistribution {

  /*
    lambda = the average number of success that occur in a particular region
    k = the actual number of successes that occur in a particular region
   */
  def probabilityMassFunction(k : Int, lambda: Double) = {
    math.pow(lambda, k) * math.exp(-1 * lambda) / factorial(k)
  }

  def poissonInRange(start: Int, end: Int, lambda: Int): Double = {

    def aux(current: Int, sum: Double): Double = {
      if (current > end) sum
      else aux(current + 1, sum + probabilityMassFunction(current, lambda))
    }
    aux(start, 0)
  }

}
