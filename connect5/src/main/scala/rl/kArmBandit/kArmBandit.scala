package rl.kArmBandit

import breeze.linalg._
import breeze.plot._
import breeze.linalg.DenseVector
import breeze.stats.distributions.Binomial
import java.awt.{Color, Paint}
import breeze.numerics._

/*
  * Created by wangmich on 10/30/2017.
  */
class Bandit (kArm: Int = 10, epsilon:Double = 0.0, stepSize:Double = 0.0) {
  //val estimation = DenseVector[Double](10).map(_ => math.random)
  val estimation = DenseVector.zeros[Double](10).map(_ => math.random)
  val qEstimation = DenseVector.zeros[Double](10)
  val actionCount = Array[Int](kArm)
  var time = 0
  var averageReward = 0.0

  def getAction = epsilon match {
    case 0 => argmax(qEstimation)
    case _ => if (Binomial(1, epsilon).draw == 1) scala.util.Random.nextInt(10) else  argmax(estimation)
  }
  def takeAction(arm:Int) = {
    val reward = estimation.valueAt(arm) + math.random
    time += 1
    averageReward = (time -1)/time * averageReward + reward/time
    actionCount(arm) = actionCount(arm)+1
    //qEstimation(arm) += stepSize * (reward - qEstimation(arm))
    qEstimation(arm) += 1.0/actionCount(arm) * (reward - qEstimation(arm))
    reward
  }
  def bestAction = argmax(qEstimation)
}

object kArmBandit extends App{

  test
  epsilonGreedy(10, 100)

  private def test = {




    //scatter(x, y, s)


  }
  def epsilonGreedy(nBandits:Int, time:Int) = {
    val epsilon = Seq(0, 0.1, 0.01)
    val bandits= (new Array[Bandit](nBandits)).map(_ => new Bandit(10, 0, 0.1))
    val (bestActionCount, averageRewards) = banditSimulation(nBandits, time, bandits)
    val f = Figure()
    val p = f.subplot(0)
    //val x = averageRewards(::, 0)
    //val y = averageRewards(::, 1)
    //p += scatter(x, y, { _ => 0.1 }, { a => Color.BLUE })

    for (i <- 0 to time -1) {
      p += scatter(DenseVector.fill[Double](nBandits, i), averageRewards(::, i), { _ => 0.1 }, { a => Color.BLUE })
    }
    p.xlabel = "X-value"
     p.ylabel = "Y-value"
     p.title = "Input data"
    //    p += plot(x,y, '.')
    //    p.xlabel = "x axis"
    //    p.ylabel = "y axis"
  }
  val y2Color: DenseVector[Double] => (Int => Paint) = y => {
    case i => i2color(y(i).toInt)
  }
  val i2color: Int => Paint = _ match {
    case 1 => Color.BLUE //accepted
    case 0 => Color.RED //rejected
    case _ => Color.BLACK //other
  }
  def banditSimulation(n:Int, time:Int, bandits:Array[Bandit]) = {
    val bestActionCounts = DenseMatrix.zeros[Int] (bandits.length, time)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, time)
    for (b <- 0 to bandits.length-1; t <-0 to time-1) {
      val action = bandits(b).getAction
      val reward = bandits(b).takeAction(action)
      averageRewards(b, t) += reward
      if (action == bandits(b).bestAction) {
        bestActionCounts(b,t) += 1
      }
    }
    (bestActionCounts, averageRewards)
  }
}
