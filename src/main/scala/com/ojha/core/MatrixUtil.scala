package com.ojha.core


object MatrixUtil {

  import MatrixImplicits._

  type Matrix = IndexedSeq[IndexedSeq[Double]]

  def inverse(matrix: Matrix): Matrix = {
    val adjugate = matrix.matrixOfMinors.matrixOfCofactors.adjugate
    val inverseDeterminant = matrix.inverseDeterminant
    adjugate.map(v1 => v1.map(v2 =>  v2 * inverseDeterminant))
  }

  def determinant(m: Matrix): Double = {
    (m.length, m.head.length) match {
      case (2,2) => m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
      case (n1, n2) => m.head.zipWithIndex.map{
        case (v, i) if i % 2 == 0 => determinant(withoutRowAndCol(m, 0, i)) * v
        case (v,i) => (determinant(withoutRowAndCol(m, 0, i)) * v) * -1
      }.sum
    }
  }

  def inverseDeterminant(matrix: Matrix): Double = 1 / matrix.determinant

  def withoutRowAndCol(matrix: Matrix, row: Int, col: Int): Matrix = {
    matrix.zipWithIndex
      .filter(pair => pair._2 != row)
      .map(_._1).map(row => row.zipWithIndex.filter(pair => pair._2 != col).map(_._1))
  }

  /*
     Given an nxn matrix, for every element with index (i,j), 'remove' the ith row and the
     jth column from the matrix and calculate the determinant of the resultant n-1 x n-1 matrix.
     The (i,j)th element of the resultant matrix is this determinant.
   */
  def matrixOfMinors(m: Matrix): Matrix = {
    m.indices.map(row => m.head.indices.map(col => determinant(withoutRowAndCol(m, row, col))))
  }

  /*
     Given a matrix of minors multiply each value by +1 or -1 according to the following pattern:

     +1 -1 +1 -1 ...
     -1 +1 -1 +1 ...
     +1 -1 +1 -1 ...
     ...

   */
  def matrixOfCofactors(m: Matrix): Matrix = {
    m.indices.map(rowIndex => m.head.indices.map(colIndex => {
      colIndex + rowIndex match {
        case i if (i%2) == 0 => m(rowIndex)(colIndex)
        case _ => m(rowIndex)(colIndex) * -1
      }
    }))
  }

  /*
    Reflect matrix across the diagonal axis
   */
  def adjugate(m: Matrix): Matrix = {
    m.indices.map(rowIndex => m.head.indices.map(colIndex => {
      (rowIndex, colIndex) match {
        case (r,c) if r == c => m(r)(c)
        case (r,c) => m(c)(r)
      }
    }))
  }

  def multiply(m1: Matrix, m2: Matrix): Matrix = {
    m1.indices.map(row => m2.head.indices.map(col =>  (m1(row), m2.map(m => m(col))).zipped.map(_ * _ ).sum ))
  }

  def transpose(x: Matrix): Matrix = x.head.indices.map(i => x.map(a => a(i)).toIndexedSeq )

}
