package com.ojha.core

object NormalEquation extends App {

  val x1 = Array(5,6,7,8,9)
  val x2 = Array(7,6,4,5,6)
  val y = Array(10,20,60,40,50)

  meow(Array(x1, x2))

  def meow(xs: Array[Array[Int]]) = {
    val ones = Array.fill(x1.length)(1)
    val emwo = ones.zip(x1)
    val rawr = emwo.zip(x2)

    val meow = rawr

  }



}
