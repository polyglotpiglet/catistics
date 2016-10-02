package com.ojha.core

import MathUtil._

class NegativeBinomialDistribution {

  /*
    let x = numberSuccesses
    let n = totalNumberOfTrials
    this function gives the probability of having x-1 successes after n-1 trials and x successes after n trials
    this is the negative binomial probability
   */
  def probabilityMassFunction(numberSuccesses: Int, totalNumberOfTrials: Int, probabilitySuccess: Double) = {

    val numberFailures = totalNumberOfTrials - numberSuccesses
    val probabilityFailure = 1 - probabilitySuccess

    choose(totalNumberOfTrials - 1, numberSuccesses - 1) * math.pow(probabilitySuccess, numberSuccesses) * math.pow(probabilityFailure, numberFailures)
  }

}
