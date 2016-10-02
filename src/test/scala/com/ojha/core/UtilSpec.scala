package com.ojha.core

import org.scalatest.FlatSpec
import MathUtil._

class UtilSpec extends FlatSpec {

  it should "calculate factorial" in {
    assert(factorial(0) == 1)
    assert(factorial(1) == 1)
    assert(factorial(2) == 2)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
  }

  it should "choose n and x" in {
    assert(choose(3,2) == 3)
    assert(choose(8,3) == 56)
  }


}
