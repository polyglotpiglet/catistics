package com.ojha.core

import MathUtil._

object BinomialDistribution {

  def probabilityMassFunction(numberOfSuccessfulTrials: Int,
                              totalNumberOfTrials: Int,
                              probabilitySuccess: Double): Double = {

    val probabilityFailure = 1 - probabilitySuccess
    val numberOfFailures = totalNumberOfTrials - numberOfSuccessfulTrials
    choose(totalNumberOfTrials, numberOfSuccessfulTrials) * math.pow(probabilitySuccess, numberOfSuccessfulTrials) * math.pow(probabilityFailure, numberOfFailures)
  }

  def probabilityOfNumberOfSuccessesInRange(startRange: Int,
                                            endRange: Int,
                                            totalNumberOfTrials: Int,
                                            probabilityOfSuccess: Double
                                           ) = {

    def aux(current: Int, end: Int, sum: Double): Double = end - current match {
      case -1 => sum
      case _ => aux(current + 1, end, sum + probabilityMassFunction(current, totalNumberOfTrials, probabilityOfSuccess))
    }

    aux(startRange, endRange, 0)
  }


}
