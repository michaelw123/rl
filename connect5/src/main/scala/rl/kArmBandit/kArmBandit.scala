package rl.kArmBandit

import breeze.linalg._
import breeze.plot._
import breeze.linalg.DenseVector
import breeze.stats.distributions.Binomial

/*
  * Created by wangmich on 10/30/2017.
  */
class Bandit (kArm: Int = 10, epsilon:Double = 0.0, stepSize:Double = 0.0) {
  val estimation = DenseVector[Double](10)
  val qEstimation = DenseVector[Double](10)
  val actionCount = Array[Int](kArm)
  var time = 0
  var averageReward = 0.0

  def getAction = epsilon match {
    case 0 => argmax(estimation)
    case _ => if (Binomial(1, epsilon).draw == 1) scala.util.Random.nextInt(10) else  argmax(estimation)
  }
  def takeAction(arm:Int) = {
    val reward = math.random
    time += 1
    averageReward = (time -1)/time * averageReward + reward/time
    actionCount(arm) = actionCount(arm)+1
    qEstimation(arm) = stepSize * (reward - qEstimation(arm))
    reward
  }
  def bestAction = argmax(qEstimation)
}

object kArmBandit extends App{

  //test
  epsilonGreedy(10, 10)

  private def test = {
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0,9.0, 10)
    val y = x +:+ 2.0
    p += plot(x, x :^ 2.0)
    p += plot(x, x :^ 3.0, '.')
    p += plot(x,y, '.')

    p.xlabel = "x axis"
    p.ylabel = "y axis"
    f.saveas("lines.png")
  }
  def epsilonGreedy(nBandits:Int, time:Int) = {
    val epsilon = Seq(0, 0.1, 0.01)
    val bandits= (new Array[Bandit](nBandits)).map(_ => new Bandit(10, 0, 0))
    val bb = bandits.map(_ => new Bandit(10, 0, 0))
    val (bestActionCount, agerageRewards) = banditSimulation(nBandits, time, bandits)
    val f = Figure()
    val p = f.subplot(0)
    val x = linspace(0.0,9.0, 10)
    val y = x +:+ 2.0
    val rows = bestActionCount(*, ::)
    for (row <- rows) {
      p += plot(x,row, '.')
      //val x = 0
    }
    p += plot(x,y, '.')
    p.xlabel = "x axis"
    p.ylabel = "y axis"
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
