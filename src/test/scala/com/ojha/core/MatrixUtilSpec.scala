package com.ojha.core

import org.scalatest.FlatSpec

class MatrixUtilSpec extends FlatSpec {

  it should "transpose a 2x5 matrix into 5x2" in {
    val x = IndexedSeq(
      IndexedSeq(5,6,7,8,9),
      IndexedSeq(7,6,4,5,6)
    )

    val transpose = IndexedSeq(
      IndexedSeq(5,7),
      IndexedSeq(6,6),
      IndexedSeq(7,4),
      IndexedSeq(8,5),
      IndexedSeq(9,6)
    )

    assert(transpose == MatrixUtil.transpose(x))
  }

  it should "multiply 2x2 matrix" in {
    val m1 = IndexedSeq(
      IndexedSeq(1,2),
      IndexedSeq(3,4)
    )

    val m2 = IndexedSeq(
      IndexedSeq(2,0),
      IndexedSeq(1,2)
    )

    val expected = IndexedSeq(
      IndexedSeq(4,4),
      IndexedSeq(10,8)
    )

    assert(expected == MatrixUtil.multiply(m1, m2))
  }

  it should "multiply 3x2 and 2x4 matrix" in {
    val m1 = IndexedSeq(
      IndexedSeq(1,4),
      IndexedSeq(0,1),
      IndexedSeq(-1,0)
    )

    val m2 = IndexedSeq(
      IndexedSeq(4,1,2,1),
      IndexedSeq(0,1,-1,3)
    )

    val expected = IndexedSeq(
      IndexedSeq(4,5,-2,13),
      IndexedSeq(0,1,-1,3),
      IndexedSeq(-4,-1,-2,-1)
    )

    assert(expected == MatrixUtil.multiply(m1, m2))
  }

  it should "calculate the determinant of a 2x2 matrix" in {
    val m = IndexedSeq(
      IndexedSeq(4,6),
      IndexedSeq(3,8)
    )

    assert(14 == MatrixUtil.determinant(m))
  }

  it should "calculate the determinant of a 3x3 matrix" in {
    val m = IndexedSeq(
      IndexedSeq(3,0,2),
      IndexedSeq(2,0,-2),
      IndexedSeq(0,1,1)
    )

    assert(10 == MatrixUtil.determinant(m))
  }

  it should "calculate the matrix of minors of a 3x3 matrix" in {
    val m = IndexedSeq(
      IndexedSeq(3,0,2),
      IndexedSeq(2,0,-2),
      IndexedSeq(0,1,1)
    )

    val expectedMinor = IndexedSeq(
      IndexedSeq(2,2,2),
      IndexedSeq(-2,3,3),
      IndexedSeq(0,-10,0)
    )

    assert(expectedMinor == MatrixUtil.matrixOfMinors(m))
  }

  it should "calculate the matrix of cofactors of a 3x3 matrix" in {
    val m = IndexedSeq(
      IndexedSeq(2,2,2),
      IndexedSeq(-2,3,3),
      IndexedSeq(0,-10,0)
    )

    val expectedMinor = IndexedSeq(
      IndexedSeq(2,-2,2),
      IndexedSeq(2,3,-3),
      IndexedSeq(0,10,0)
    )

    assert(expectedMinor == MatrixUtil.matrixOfCofactors(m))
  }



}
