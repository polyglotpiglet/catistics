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

  val x = xT.transpose
  val y = yT.transpose
  (((xT dot x).inverse dot xT) dot y).print

}
