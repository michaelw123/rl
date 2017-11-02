package rl.kArmBandit

import breeze.linalg._
import breeze.plot._
import breeze.linalg.operators
import breeze.linalg.DenseVector

/*
  * Created by wangmich on 10/30/2017.
  */
class Bandit (index: Int = 0) {
 // def getAction:Double = math.random()
  def takeAction(b:Int) = b match {
    case 0 => 0 // sample Average
    case 1 => 1 // gradiant
    case _ => 2 // update estimate
  }
}

object kArmBandit extends App{

  //test
  val f = Figure()
  val p = f.subplot(0)
  val x = linspace(0.0,1.0)
  val y = x :+= 2.0
  p += plot(x, x :^ 2.0)
  p += plot(x, x :^ 3.0, '.')
  p += plot(x,y, '.')

  p.xlabel = "x axis"
  p.ylabel = "y axis"
  f.saveas("lines.png")
//  val p = f.subplot(0)
//  val g = breeze.stats.distributions.Gaussian(0,1)
//  p += hist(g.sample(10000), 100)
//  p.title = "A normal distribution"

  //val f2 = Figure()
  //f2.subplot(0) += image(DenseMatrix.rand(200,200))
  //f.saveas("subplots.png")
//  private def test = {
//    val f = Figure()
//    val p = f.subplot(0)
//    val x = linspace(0.0,1.0)
//    p += plot(x, x :^ 2, '.')
//    p.xlabel = "x axis"
//    p.ylabel = "y axis"
//  }

}
