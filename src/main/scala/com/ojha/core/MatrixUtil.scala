package com.ojha.core


object MatrixUtil {
  def determinant[T](m: IndexedSeq[IndexedSeq[T]])(implicit num: Numeric[T]): T = {
    import num._
    (m.length, m.head.length) match {
      case (2,2) => m(0)(0) * m(1)(1) - m(0)(1) * m(1)(0)
      case (n1, n2) =>m(0)(0)
    }
  }

  /*
     Given an nxn matrix, for every element with index (i,j), 'remove' the ith row and the
     jth column from the matrix and calculate the determinant of the resultant n-1 x n-1 matrix.
     The (i,j)th element of the resultant matrix is this determinant.
   */
  def matrixOfMinors[T](m: IndexedSeq[IndexedSeq[T]])(implicit num: Numeric[T]): IndexedSeq[IndexedSeq[T]] = {
    m.indices.map(rowIndex => m.head.indices.map(colIndex => {
      val withoutRowAndCol = m.zipWithIndex
        .filter(pair => pair._2 != rowIndex)
        .map(_._1).map(row => row.zipWithIndex.filter(pair => pair._2 != colIndex).map(_._1))
      determinant(withoutRowAndCol)
    }))
  }

  /*
     Given a matrix of minors multiply each value by +1 or -1 according to the following pattern:

     +1 -1 +1 -1 ...
     -1 +1 -1 +1 ...
     +1 -1 +1 -1 ...
     ...

   */
  def matrixOfCofactors[T](m: IndexedSeq[IndexedSeq[T]])(implicit num: Numeric[T]): IndexedSeq[IndexedSeq[T]] = {
    import num._
    m.indices.map(rowIndex => m.head.indices.map(colIndex => {
      colIndex + rowIndex match {
        case i if (i%2) == 0 => m(rowIndex)(colIndex)
        case _ => (m(rowIndex)(colIndex).toDouble() * -1).asInstanceOf[T]
      }
    }))
  }


  def multiply[T](m1: IndexedSeq[IndexedSeq[T]], m2: IndexedSeq[IndexedSeq[T]])(implicit num: Numeric[T]): IndexedSeq[IndexedSeq[T]] = {
    import num._
    m1.indices.map(row => m2.head.indices.map(col =>  (m1(row), m2.map(m => m(col))).zipped.map(_ * _ ).sum ))
  }

  def transpose[A](x: IndexedSeq[IndexedSeq[A]]): IndexedSeq[IndexedSeq[A]] = x.head.indices.map(i => x.map(a => a(i)).toIndexedSeq )
}
