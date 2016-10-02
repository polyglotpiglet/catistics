package com.ojha.core

import org.scalatest.FlatSpec

class GeometricDistributionSpec extends FlatSpec {

  it should "calculate probability that success, which has 70% probability, occurs on the 5th attempt for the first time" in {
    assert(GeometricDistribution.probabilityMassFunction(5, 0.7) == 0.005670000000000003)
  }
}
