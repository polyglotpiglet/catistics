package com.ojha.core

import org.scalatest.FlatSpec

class PoissonDistributionSpec extends FlatSpec {

  it should "return probability that we will sell exactly 3 homes tomorrow given we sell on average 2 per day" in {
    assert(PoissonDistribution.probabilityMassFunction(3, 2) == 0.1804470443154836 )
  }

  it should "return probability that we see 0,1,2 or 3 lions on a one-day safari given the average is 5 lions per day" in {
    assert(PoissonDistribution.poissonInRange(0, 3, 5) == 0.2650259152973617)
  }

  it should "" in {
    println(160 + 40 * math.pow(0.88, 2))
    println(128 + 40 * math.pow(1.55, 2))
  }

}
