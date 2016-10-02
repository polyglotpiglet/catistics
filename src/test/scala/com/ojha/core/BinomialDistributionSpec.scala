package com.ojha.core

import org.scalatest.FlatSpec

class BinomialDistributionSpec extends FlatSpec {

  it should "return the probablity of getting 0 heads when you toss a coin 10 times" in {
    assert(BinomialDistribution.probabilityMassFunction(0, 10, 0.5) == 9.765625E-4)
  }

  it should "return the probablity of getting 5 heads when you toss a coin 10 times" in {
    assert(BinomialDistribution.probabilityMassFunction(5, 10, 0.5) == 0.24609375)
  }

  it should "return the probablity of getting at least 5 heads when you toss a coin 10 times" in {
    assert(BinomialDistribution.probabilityOfNumberOfSuccessesInRange(5, 10, 10, 0.5) == 0.623046875)
  }

  it should "return the probablity of getting at most 5 heads when you toss a coin 10 times" in {
    assert(BinomialDistribution.probabilityOfNumberOfSuccessesInRange(0, 5, 10, 0.5) == 0.623046875)
  }

}
