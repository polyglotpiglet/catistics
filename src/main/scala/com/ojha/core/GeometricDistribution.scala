package com.ojha.core

import MathUtil._

object GeometricDistribution {

  def probabilityMassFunction(numberOfTrials: Int, probabilitySuccess: Double): Double = {
    val probabilityFailure = 1 - probabilitySuccess
    math.pow(probabilityFailure, numberOfTrials - 1) * probabilitySuccess
  }



}
