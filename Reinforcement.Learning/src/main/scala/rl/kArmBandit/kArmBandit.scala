package rl.kArmBandit

import breeze.linalg._
import breeze.plot._
import breeze.stats.distributions.Binomial
import java.awt.{Color, Paint}


/*
  * Created by wangmich on 10/30/2017.
  */
class Bandit (kArm: Int = 10, epsilon:Double = 0, stepSize:Double = 0.1) {
  val estimation = DenseVector.zeros[Double](kArm).map(_ => math.random)
  //val estimation = DenseVector.fill[Double](kArm)(math.random)
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
  def bestAction = argmax(qEstimation)
}

object kArmBandit extends App{

  test
  epsilonGreedy(100, 50)

  private def test = {
  }
  def epsilonGreedy(nBandits:Int, time:Int) = {
    val epsilons = Seq(0, 0.1, 0.01)
    val colors = Seq("BLUE", "RED", "BLACK")
    val f = Figure()
    val p = f.subplot(0)
    p.xlabel = "Steps"
    p.ylabel = "Average Rewards"
    p.title = "Input data"
    for (epslon <- epsilons) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, epslon, 0.1))
      val (bestActionCount, average) = banditSimulation(nBandits, time + 1, bandits)
      val color = epslon match {
        case 0 => "BLACK"
        case 0.1 => "RED"
        case 0.01 => "BLUE"
        case _ => "BLACK"
      }
    // val aaa=average1(0)
      //for(col <- average(*, ::)) {
      for (row <- average(::, *)) {
      //val lin = linspace(0, time+1, time+1)
      //p+=scatter(linspace(0, time+1, time+1), col, { _ => 0.3 }, { a => Color.BLUE })
           p += plot(linspace(0, nBandits, nBandits), row, colorcode= color)
      //p += scatter(DenseVector.fill[Double](nBandits, step), col, { _ => 0.3 }, { a => Color.BLUE })
      }

    }
    val xxx=8
    p.refresh()
//    val p1 = f.subplot(0)
//    step = 0
//    for(col <- best
    // ActionCount(::, *)) {
//      p1 += scatter(DenseVector.fill[Double](nBandits, step), col, { _ => 0.3 }, { a => Color.BLUE })
//      step += 1
//    }
//    p1.xlabel = "Steps"
//    p1.ylabel = "Best Actions (%)"
//    p1.title = "Input data"
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
    val bestActionCounts = DenseMatrix.zeros[Double] (bandits.length, time)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, time)
    for (bandit <- bandits) {
      for (i <- 0 to n-1) {
        for (t <- 1 to time - 1) {
          val action = bandit.getAction
          val reward = bandit.takeAction(action)
          averageRewards(i, t) += reward
          if (action == bandit.bestAction) {
            bestActionCounts(i, t) += 1
          }
        }
      }
    }
    (bestActionCounts.map(_/bandits.length), averageRewards.map(_/bandits.length) )
  }
}
