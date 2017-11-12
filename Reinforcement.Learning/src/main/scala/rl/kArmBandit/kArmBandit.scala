package rl.kArmBandit

import breeze.linalg._
import breeze.stats._
import breeze.plot._
import breeze.stats.distributions.Binomial

/*
  * Created by wangmich on 10/30/2017.
  */
class Bandit (kArm: Int = 10, epsilon:Double = 0, stepSize:Double = 0.1) {
  val estimation = DenseVector.zeros[Double](kArm).map(_ => math.random)
  val qEstimation = DenseVector.zeros[Double](kArm)
  val actionCount = Array.fill[Int](kArm)(0)
  var time = 0
  var averageReward = 0.0

  def getAction = //scala.util.Random.nextInt(10)
    epsilon match {
    case 0 => argmax(qEstimation)
    case _ => if (Binomial(1, epsilon).draw == 1) scala.util.Random.nextInt(kArm) else  argmax(qEstimation)
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
  def bestAction = argmax(estimation)
}
object kArmBandit extends App{
  epsilonGreedy(1000, 2000)

  def epsilonGreedy(nBandits:Int, time:Int) = {
    val epsilons = Seq(0,0.001, 0.01, 0.1, 0.4)
    val colors = Seq("BLUE", "RED", "BLACK")

    for (epslon <- epsilons) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, epslon, 0.1))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val color = epslon match {
        case 0 => "BLACK"
        case 0.1 => "RED"
        case 0.01 => "BLUE"
        case 0.001 => "YELLOW"
        case _ => "RED"
      }
      val f = Figure()
      val p = f.subplot(0)

//      p += plot(linspace(0, time+1, time+1), mean(average(::, *)).inner, colorcode= color)
//
//      p.xlabel = "Steps"
//      p.ylabel = "Average Rewards"
//      p.title = "epsolon ="+epslon

      val p1 = f.subplot(0)

      p1 += plot(linspace(0, time+1, time+1), mean(bestActions(::, *)).inner, colorcode= color)

      p1.xlabel = "Steps"
      p1.ylabel = "Best Action %"
      p1.title = "epsolon ="+epslon
      


//      val p1 = f.subplot(1)
//      for (col <- bestActionCount(::, *)) {
//        p1 += plot(linspace(0, nBandits, nBandits), col, colorcode= color)
//      }
//      p1.xlabel = "Steps"
//      p1.ylabel = "Best Action Count"
//      p1.title = "epsolon ="+epslon
    }
  }

  def banditSimulation(n:Int, time:Int, bandits:Array[Bandit]) = {
    val bestActionCounts = DenseMatrix.zeros[Double] (bandits.length, time)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, time)
    for (i <- 0 to n-1) {
        for (t <- 1 to time - 1) {
          val bandit = bandits(i)
          val action = bandit.getAction
          val reward = bandit.takeAction(action)
          averageRewards(i, t) += reward
          if (action == bandit.bestAction) {
            bestActionCounts(i, t) += 1
          }
        }
    }
    (bestActionCounts, averageRewards.map(_/bandits.length) )

  }
}
