package com.ojha.core

import java.io._

object GradientDescent extends App {

  //Theta1: 0.238855421686753, Theta2: 0.7183734939758966

//  val xs = Array(1,1.1,0.8,0.2,1.2,0.9,1.1, 1.2,0.4, 0.7, 0.8, 0.6)
//  val ys = Array(0.8, 1.0, 0.7, 0.4, 1.1, 1.1, 1.3, 1.0, 0.5, 0.8, 0.6, 0.75)

//  val xs = Array(1,2,3,4,5.0)
//  val ys = Array(2,1,4,3,5.0)

  val xs = Array(95, 85, 80, 70, 60).map(_ / 100.0)
  val ys = Array(85, 95, 70, 65, 70).map(_ / 100.0)

  def computeCost(theta1: Double, theta2: Double): Double = {
    val hypothesises = xs.map(x => theta1 + theta2 * x)
    val summedSquareDiffs = (hypothesises, ys).zipped
      .map(_ - _)
      .map(math.pow(_, 2))
      .sum
    summedSquareDiffs / (2 * xs.length)
  }

  val file = new File("/Users/alexbate/polyglotpiglet/blogs/gradientDescent/results/alphaZeroPoint1AndIterationsEquals100.txt")
  val bw = new BufferedWriter(new FileWriter(file))

  val alpha = 0.1
  val (theta1, theta2) = (0 to 1000000).foldLeft((0.0, 0.0)){ case ((t1, t2), _) => {
//    bw.write(computeCost(t1, t2).toString + "\n")
    (nextTheta1(t1, t2, alpha, xs, ys), nextTheta2(t1, t2, alpha, xs, ys))
  }}
  bw.close()

  println(s"Theta1: $theta1, Theta2: $theta2")
  println(theta1 + theta2 * 0.8)

  def y(theta1: Double, theata2: Double, x: Double): Double = theta1 + theata2 * x

  def nextTheta1(theta1: Double, theta2: Double, alpha: Double, xs: Array[Double], ys: Array[Double]): Double = {
    val partial = xs.zip(ys).map(pair => y(theta1, theta2, pair._1) - pair._2).sum / xs.length
    theta1 - alpha * partial
  }

  def nextTheta2(theta1: Double, theta2: Double, alpha: Double, xs: Array[Double], ys: Array[Double]): Double = {
    val partial = xs.zip(ys).map(pair => (y(theta1, theta2, pair._1) - pair._2) * pair._1).sum / xs.length
    theta2 - alpha * partial
  }


}
