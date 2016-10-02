package com.ojha.core

import com.ojha.core.MatrixUtil.Matrix
import MatrixImplicits._
import MatrixUtil._

object NormalEquation extends App {

  val xT: Matrix = IndexedSeq(
    IndexedSeq(1,1,1,1,1),
    IndexedSeq(5,6,7,8,9),
    IndexedSeq(7,6,4,5,6)
  )

  val yT: Matrix = IndexedSeq(IndexedSeq(10,20,60,40,50))

  def solve(x: Matrix, y: Matrix): Matrix = {
    val xT = x.transpose
    val yT = y.transpose
    ((xT dot x).inverse dot xT) dot y
  }



  val X: Matrix = IndexedSeq(IndexedSeq(1, 1.1, 0.8, 0.2, 1.2, 0.9, 1.1, 1.2, 0.4, 0.7, 0.8, 0.6))
  val Y: Matrix = IndexedSeq(IndexedSeq(0.8, 1.0, 0.7, 0.4, 1.1, 1.1, 1.3, 1.0, 0.5, 0.8, 0.6, 0.75))

  solve(X, Y).print


}
