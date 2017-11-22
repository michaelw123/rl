/*
 * Copyright (c) 2016 Michael Wang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package rl.kArmBandit

import breeze.linalg._
import breeze.numerics.{exp, log, sqrt}
import breeze.stats._
import breeze.plot._
import breeze.stats.distributions.{Binomial, Rand, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister

import scala.annotation.tailrec

/*
  * Created by Michael Wang on 10/30/2017.
  */
class Bandit (kArm: Int = 10, epsilon:Double = 0, stepSize:Double = 0.1, incremental:Boolean=false, initial:Double = 0, ucb:Int = 0, gradient:Int=0) {

  val trueQ = DenseVector.fill[Double](kArm)(math.random)
  val qEstimation = DenseVector.fill[Double](kArm)(initial)
  val actionCount = Array.fill[Int](kArm)(0)
  var actionProb = DenseVector.zeros[Double](kArm)
  var time = 0
  var averageReward = 0.0
  def getAction:Int = //scala.util.Random.nextInt(10)
    (epsilon, ucb, gradient) match {
      case (0, 0, 0) => argmax(qEstimation) //epsilon == 0
      case (_, 0, 0) => if (Binomial(1, epsilon).draw == 1) scala.util.Random.nextInt(kArm) else  argmax(qEstimation)  // epsilon != 0
      case (_, _, 0) => argmax( //ucb
        for ((est, count) <- qEstimation.toArray zip actionCount) yield  est + ucb * sqrt(log(time+1)/(count+1))
      )
      case (_, _, _) => {//gradient
        val expEstimation = qEstimation.map(exp(_))
        val theSum:Double = sum(expEstimation)
        actionProb = expEstimation.map(a => a/theSum)
        val index:Int = ExtendedRand.weightedChooseIndex(qEstimation.toArray, actionProb.toArray).draw
        index
      }
  }


//  elif self.gradient:
//    oneHot = np.zeros(self.k)
//  oneHot[action] = 1
//  if self.gradientBaseline:
//    baseline = self.averageReward
//  else:
//  baseline = 0
//  self.qEst = self.qEst + self.stepSize * (reward - baseline) * (oneHot - self.actionProb)
  def takeAction(arm:Int):Double = {
    val reward = trueQ.valueAt(arm) + math.random
    time += 1
    averageReward = (time -1)/time * averageReward + reward/time
    actionCount(arm) = actionCount(arm)+1
    if (incremental) {
      qEstimation(arm) += stepSize * (reward - qEstimation(arm)) //constant stepsize
    } else if (gradient != 0) {
      val oneHot = DenseVector.zeros[Double](kArm)
      oneHot(arm) = 1
      qEstimation += (oneHot - actionProb) * stepSize * reward
    }else {
        qEstimation(arm) += 1.0 / actionCount(arm) * (reward - qEstimation(arm))
    }
    reward
  }
  def bestAction:Int = argmax(trueQ)
}


object kArmBandit extends App{
  //ucbEpsilonGreedy(1000, 4000)
  //incrementalEpsilonGreedy(1000, 4000)
  //epsilonGreedyAverageRewards(1000, 4000)
  //epsilonGreedyBestActions(1000, 4000)
  //optimisticEpsilonGreedy(1000, 6000)
  gradientEpsilonGreedy(1000, 6000)

  def ucbEpsilonGreedy(nBandits:Int, time:Int) = {
   val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, 0.1, 0.1, true, 0, 2))
   val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
   val f = Figure()
   val p = f.subplot(0)
   p += plot(linspace(0, time+1, time+1), mean(average(::, *)).inner, colorcode=color(0.1))
   p.xlabel = "Steps"
   p.ylabel = "Best average with initial value of "+0
   p.title = "epslon ="+0.1
 }
  def optimisticEpsilonGreedy(nBandits:Int, time:Int) = {
    val epsilons = Seq(0, 0.005, 0.01, 0.1)
    val initails = Seq(0, 1, 5)
    for (initial <- initails) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, 0.1, 0.1, true, initial))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(linspace(0, time+1, time+1), sum(bestActions(::, *)).inner, colorcode=color(0.1))
      p.xlabel = "Steps"
      p.ylabel = "Best Action count with initial value of "+initial
      p.title = "epslon ="+0.1
    }

  }
  def incrementalEpsilonGreedy(nBandits:Int, time:Int) = {
    val epsilons = Seq(0, 0.005, 0.01, 0.1)
    for (epslon <- epsilons) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, epslon, 0.2, true))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(linspace(0, time+1, time+1), sum(bestActions(::, *)).inner, colorcode=color(epslon))
      p.xlabel = "Steps"
      p.ylabel = "Best Action count"
      p.title = "epslon ="+epslon
    }

  }
  def epsilonGreedyBestActions(nBandits:Int, time:Int) = {
    val epsilons = Seq(0.005, 0.01, 0.1)
    for (epslon <- epsilons) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, epslon, 0.1))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(linspace(0, time+1, time+1), sum(bestActions(::, *)).inner, colorcode=color(epslon))
      p.xlabel = "Steps"
      p.ylabel = "Best Action count"
      p.title = "epslon ="+epslon
    }
  }
  def epsilonGreedyAverageRewards(nBandits:Int, time:Int) = {
    val epsilons = Seq(0.005, 0.01, 0.1)
    val colors = Seq("BLUE", "RED", "BLACK")

    for (epslon <- epsilons) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, epslon, 0.1))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(linspace(0, time+1, time+1), mean(average(::, *)).inner, colorcode=color(epslon))
      p.xlabel = "Steps"
      p.ylabel = "Average Rewards"
      p.title = "epslon ="+epslon
    }
  }
  private def  color(epslon:Double):String = epslon match {
    case 0 => "BLACK"
    case 0.1 => "RED"
    case 0.01 => "BLUE"
    case 0.001 => "YELLOW"
    case _ => "RED"
  }
  def banditSimulation(n:Int, time:Int, bandits:Array[Bandit]) = {
    val bestActionCounts = DenseMatrix.zeros[Double] (bandits.length, time)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, time)
    for (i <- 0 until n; t <- 1 until time ) {
          val bandit = bandits(i)
          val action:Int = bandit.getAction
          val reward = bandit.takeAction(action)
          averageRewards(i, t) += reward
          if (action == bandit.bestAction) bestActionCounts(i, t) += 1
    }
    (bestActionCounts, averageRewards.map(_/bandits.length) )
  }
  def gradientEpsilonGreedy(nBandits:Int, time:Int) = {
    for (stepSize <- Seq(0.01, 0.1, 0.4)) {
      val bandits = (new Array[Bandit](nBandits)).map(_ => new Bandit(10, 0.1, stepSize, gradient = 2))
      val (bestActions, average) = banditSimulation(nBandits, time + 1, bandits)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(linspace(0, time + 1, time + 1), sum(bestActions(::, *)).inner, colorcode = color(0.1))
      p.xlabel = "Steps"
      p.ylabel = "Average Rewards"
      p.title = "stepSize =" + stepSize
    }
  }
}

object ExtendedRand extends RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister())) {
  def weightedChoose[T](c: Iterable[T], w:Iterable[Double]):Rand[T] = new Rand[T] {
    var index = 0
    val sample1 = Rand.uniform.draw
    def draw() = {
      val weightsArray = w.toArray
      val d = go(0.0, weightsArray)
      val elems = c.iterator
      var i = 1
      var e = elems.next()
      while(i < d) {
        e = elems.next()
        i += 1
      }
      e
    }
    @tailrec
    def go(s:Double, a:Array[Double]):Int = {
      index += 1
      if (s+a.head > sample1) index
      else go(s+a.head, a.tail)
    }
  }
  def weightedChooseIndex[T](c: Iterable[T], w:Iterable[Double]):Rand[Int] = new Rand[Int] {
      var index = 0
      val sample1 = Rand.uniform.draw
      def draw() = {
        val weightsArray = w.toArray
        val d = go(0.0, weightsArray)
        val elems = c.iterator
        var i:Int = 1
        var e = elems.next()
        while (i < d) {
          e = elems.next()
          i += 1
        }
        i
      }
      @tailrec
      def go(s: Double, a: Array[Double]): Int = {
        if (s + a.head > sample1) index
        else {
          index += 1
          go(s + a.head, a.tail)
        }
      }
    }
}

