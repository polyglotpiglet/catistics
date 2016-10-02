package com.ojha.core


object StatusUtil extends App {

  def stdDeviation(values: Array[Double]): Double = {
    val m = mean(values)
    val variance = values.map(v => math.pow(v - m,  2)).sum / values.length
    math.sqrt(variance)
  }

  def mean(values: Array[Double]): Double = values.sum / values.length

  def covariance(x: Array[Double], y: Array[Double]) = {
    val xBar = mean(x)
    val yBar = mean(y)
    val values = (x.map(_ - xBar), y.map(_ - yBar)).zipped.map(_ * _)
    values.sum / x.length
  }

  def pearsonRankCoefficient(x: Array[Double], y: Array[Double]) = {
    covariance(x, y) / (stdDeviation(x) * stdDeviation(y))
  }

  def spearmanRankCorrelation(x: Array[Double], y: Array[Double]) = {
    pearsonRankCoefficient(rank(x).map(_.toDouble), rank(y).map(_.toDouble))
  }

  def rank(arr: Array[Double]): Array[Int] = {
    val groupedByValue = arr.zipWithIndex.groupBy(_._1).toArray.sortBy(_._1).zipWithIndex
    val valueIndexRank = groupedByValue.flatMap{ case ((_, a), rank) => a.map{ case (double, index) => (double, index, rank + 1) }}
    valueIndexRank.sortBy(_._2).map(_._3)
  }
}
