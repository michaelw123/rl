package test.mdp

import scala.util.Random


object test extends App {
  var arguments = args map (_.toInt)

  def getPoint(): Int = {
    if (scala.math.sqrt(scala.math.pow(Random.nextDouble,2) + scala.math.pow(Random.nextDouble,2)) <= 1) 1 else 0
  }

  def getRatio(np: Int): Double = {
    (List.fill(np)(getPoint).sum * 1.0) / np
  }

//  var pi = (List.fill(arguments(0))(getRatio(arguments(1))).sum / arguments(0)) * 4.0
  //  Console.println("Approximation of Pi: " + pi)

  val aa = Array.ofDim[Double](10,10,2,4,5)
  type bb = aa.type
  println("bb")

}
