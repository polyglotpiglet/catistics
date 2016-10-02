package com.ojha.core


object MatrixUtil {

  type Matrix[T <: Numeric] = IndexedSeq[IndexedSeq[T]]

  def determinant[T](m: Matrix[T])(implicit num: Numeric[T]): T = {
    import num._
    (m.length, m.head.length) match {
      case (2,2) => m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
      case (n1, n2) => m.head.zipWithIndex.map{
        case (v, i) if i % 2 == 0 => determinant(withoutRowAndCol(m, 0, i)) * v
        case (v,i) => (determinant(withoutRowAndCol(m, 0, i)) * v) * (-1).asInstanceOf[T]
      }.sum
    }
  }

  def withoutRowAndCol[T](matrix: Matrix[T], row: Int, col: Int)(implicit num: Numeric[T]): Matrix[T] = {
    matrix.zipWithIndex
      .filter(pair => pair._2 != row)
      .map(_._1).map(row => row.zipWithIndex.filter(pair => pair._2 != col).map(_._1))
  }

  /*
     Given an nxn matrix, for every element with index (i,j), 'remove' the ith row and the
     jth column from the matrix and calculate the determinant of the resultant n-1 x n-1 matrix.
     The (i,j)th element of the resultant matrix is this determinant.
   */
  def matrixOfMinors[T](m: Matrix[T])(implicit num: Numeric[T]): Matrix[T] = {
    m.indices.map(row => m.head.indices.map(col => determinant(withoutRowAndCol(m, row, col))))
  }

  /*
     Given a matrix of minors multiply each value by +1 or -1 according to the following pattern:

     +1 -1 +1 -1 ...
     -1 +1 -1 +1 ...
     +1 -1 +1 -1 ...
     ...

   */
  def matrixOfCofactors[T](m: Matrix[T])(implicit num: Numeric[T]): Matrix[T] = {
    import num._
    m.indices.map(rowIndex => m.head.indices.map(colIndex => {
      colIndex + rowIndex match {
        case i if (i%2) == 0 => m(rowIndex)(colIndex)
        case _ => (m(rowIndex)(colIndex).toDouble() * -1).asInstanceOf[T]
      }
    }))
  }

  /*
    Reflect matrix across the diagonal axis
   */
  def adjugate[T](m: Matrix[T])(implicit num: Numeric[T]): Matrix[T] = {

    m

  }

  def multiply[T](m1: Matrix[T], m2: Matrix[T])(implicit num: Numeric[T]): Matrix[T] = {
    import num._
    m1.indices.map(row => m2.head.indices.map(col =>  (m1(row), m2.map(m => m(col))).zipped.map(_ * _ ).sum ))
  }

  def transpose[T](x: Matrix[T]): Matrix[T] = x.head.indices.map(i => x.map(a => a(i)).toIndexedSeq )
}
