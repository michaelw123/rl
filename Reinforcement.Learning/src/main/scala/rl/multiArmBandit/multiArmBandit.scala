/*
 * Copyright (c) 2017 Michael Wang
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
/*
  Implementation Details
  Assuming 10 arm bandit - this configuration can be changed easily
  arm: contains features of the bandit
  Algorithm: four althorithms are implemented by typeclass Algorithm: epsilon greedy, incremental, gradient, and ucb
  scalanlp's breeze is used for numerical computation, and breeze-viz for data visualization
  ExtendedRand is an extension of breeze Rand, which adds "weighted choose by index" from an Iterable, or typically, DenseVector.
 */

package rl.multiArmBandit

import breeze.linalg._
import breeze.numerics.{exp, log, sqrt}
import breeze.plot._
import breeze.stats.distributions.{Binomial, Rand, RandBasis, ThreadLocalRandomGenerator, Beta}
import org.apache.commons.math3.random.MersenneTwister
import breeze.stats.mean


import scala.annotation.tailrec

/*
  * Created by Michael Wang on 10/30/2017.
  */

object multiArmBandit extends App {
  trait Algorithm[T] {
    def getArm(bandit:Bandit[T]): Int
    def play(bandit:Bandit[T], arm:Int): Double
    def bestAction(bandit:Bandit[T]):Int = argmax(bandit.trueQ)
  }
  object Algorithm {
    implicit object averageGreedyAlgorithm extends Algorithm[averageGreedyArm] {
      def getArm(bandit: Bandit[averageGreedyArm]): Int = {
        if (Binomial(1, bandit.arm.epsilon).draw == 1) scala.util.Random.nextInt(bandit.k) else argmax(bandit.qEstimation)
      }

      def play(bandit: Bandit[averageGreedyArm], arm: Int): Double = {
        val reward = bandit.trueQ.valueAt(arm) + math.random
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.qEstimation(arm) += 1.0 / bandit.actionCount(arm) * (reward - bandit.qEstimation(arm))
        reward
      }
    }

    implicit object incrementalAlgorithm extends Algorithm[incrementalArm] {
      def getArm(bandit: Bandit[incrementalArm]): Int = {
        if (Binomial(1, bandit.arm.epsilon).draw == 1) scala.util.Random.nextInt(bandit.k) else argmax(bandit.qEstimation)
      }

      def play(bandit: Bandit[incrementalArm], arm: Int): Double = {
        val reward = bandit.trueQ.valueAt(arm) + math.random
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.qEstimation(arm) += 1.0 / bandit.actionCount(arm) * (reward - bandit.qEstimation(arm))
        bandit.qEstimation(arm) += bandit.arm.stepSize * (reward - bandit.qEstimation(arm))
        reward
      }
    }

    implicit object ucbAlgorithm extends Algorithm[ucbArm] {
      def getArm(bandit: Bandit[ucbArm]): Int = {
        argmax(
          for ((est, count) <- bandit.qEstimation.toArray zip bandit.actionCount) yield est + bandit.arm.ucb * sqrt(log(bandit.time + 1) / (count + 1))
        )
      }

      def play(bandit: Bandit[ucbArm], arm: Int): Double = {
        val reward = bandit.trueQ.valueAt(arm) + math.random
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.qEstimation(arm) += 1.0 / bandit.actionCount(arm) * (reward - bandit.qEstimation(arm))
        reward
      }
    }

    implicit object gradientAlgorithm extends Algorithm[gradientArm] {
      def getArm(bandit: Bandit[gradientArm]): Int = {
        val expEstimation = bandit.qEstimation.map(exp(_))
        val theSum: Double = sum(expEstimation)
        bandit.actionProb = expEstimation.map(a => a / theSum)
        val index: Int = ExtendedRand.weightedChooseIndex(bandit.qEstimation.toArray, bandit.actionProb.toArray).draw
        index
      }

      def play(bandit: Bandit[gradientArm], arm: Int): Double = {
        val reward = bandit.trueQ.valueAt(arm) + math.random + bandit.arm.trueReward
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.averageReward = (bandit.time - 1.0) / bandit.time * bandit.averageReward + reward / bandit.time
        val oneHot = DenseVector.zeros[Double](bandit.k)
        oneHot(arm) = 1
        bandit.baseline = bandit.arm.gradientaseline match {
          case true => bandit.averageReward
          case false => 0
        }
        bandit.qEstimation += (oneHot - bandit.actionProb) * bandit.arm.stepSize * (reward - bandit.baseline)
        reward
      }
    }

    //TODO: Thompson Sampling https://github.com/robertdavidwest/thompson_sampling/blob/master/thompson_sampling.py
    implicit object thomsonSamplingAlgorithm extends Algorithm[thomsonSamplingArm] {
      def getArm(bandit: Bandit[thomsonSamplingArm]): Int = {
        val index =  (bandit.k * Beta.distribution(bandit.actionCount(0), bandit.actionCount(1)).draw).toInt
//        val expEstimation = bandit.qEstimation.map(exp(_))
//        val theSum: Double = sum(expEstimation)
//        bandit.actionProb = expEstimation.map(a => a / theSum)
//        val index: Int = ExtendedRand.weightedChooseIndex(bandit.qEstimation.toArray, bandit.actionProb.toArray).draw
        index
      }

      def play(bandit: Bandit[thomsonSamplingArm], arm: Int): Double = {
        val reward = bandit.trueQ.valueAt(arm) + math.random + bandit.arm.trueReward
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.averageReward = (bandit.time - 1.0) / bandit.time * bandit.averageReward + reward / bandit.time
        val oneHot = DenseVector.zeros[Double](bandit.k)
        oneHot(arm) = 1

        reward
      }
    }

    //TODO: Baysean: https://github.com/ezraerb/BayseanBandit/blob/master/bandit.py
    implicit object bayseanAlgorithm extends Algorithm[bayseanArm] {
      def getArm(bandit: Bandit[bayseanArm]): Int = {
       (bandit.k * Beta.distribution(bandit.actionCount(0)+1, bandit.actionCount(1)+1).draw).toInt
      }

      def play(bandit: Bandit[bayseanArm], arm: Int): Double = {
        //val reward = bandit.trueQ.valueAt(arm) + math.random + bandit.arm.trueReward
        val reward = math.random
        bandit.time += 1
        bandit.actionCount(arm) = bandit.actionCount(arm) + 1
        bandit.averageReward = (bandit.time - 1.0) / bandit.time * bandit.averageReward + reward / bandit.time
        if (reward > bandit.trueQ.valueAt(arm))  bandit.trueQ(arm) = reward
        bandit.regrets = max(bandit.trueQ) - reward
        reward
      }
    }
  }
  trait Arm
  case class averageGreedyArm(epsilon:Double) extends Arm
  case class incrementalArm(epsilon:Double, stepSize: Double) extends Arm
  case class ucbArm(ucb: Int) extends Arm
  case class gradientArm(stepSize: Double = 0, trueReward:Double =0, gradientaseline:Boolean=false, baseline:Double = 0) extends Arm
  case class thomsonSamplingArm(trueReward:Double =0) extends Arm
  case class bayseanArm(trueReward:Double =0) extends Arm

  class Bandit[T](anArm:T) {
    val k:Int = 10
    var time=0
    var regrets = 0.0
    val trueQ = DenseVector.fill[Double](k)(math.random)
    val qEstimation = DenseVector.fill[Double](k)(0)
    val actionCount = Array.fill[Int](k)(0)
    var actionProb = DenseVector.zeros[Double](k)
    var averageReward:Double = 0
    var baseline:Double = 0
    def arm:T = anArm
  }
  def banditSimulation[T](n:Int, timeSteps:Int, bandits:Array[Bandit[T]])(implicit algo:Algorithm[T]) = {
    val bestActionCounts = DenseMatrix.zeros[Double] (bandits.length, timeSteps)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, timeSteps)
    val regrets = DenseVector.zeros[Double](timeSteps)
    for (i <- 0 until n; t <- 1 until timeSteps ) {
      val bandit = bandits(i)
      val arm = algo.getArm(bandit)
      val reward = algo.play(bandit, arm)
      averageRewards(i, t) += reward
      regrets(t)+= bandit.regrets
    }
    (bestActionCounts, averageRewards.map(_/bandits.length), regrets)
  }

  def epsilonGreedySimulation = {
    val timeSteps = 1000
    val f0 = Figure()
    val f1 = Figure()
    for (epsilon <- Seq(0.1, 0.2, 0.3)) {
      val bandits = Array.fill(1000)(new Bandit(averageGreedyArm(epsilon)))
      val (bestActions, average, _) = banditSimulation(1000, timeSteps, bandits)

      val p0 = f0.subplot(0)
      p0 += plot(linspace(0, timeSteps, timeSteps), sum(bestActions(::, *)).inner, colorcode = color(epsilon), name="epsilon ="+epsilon)
      p0.xlabel = "Steps"
      p0.ylabel = "Best Action"
      p0.title = "Epsilon Greedy"
      p0.legend=true

      val p1= f1.subplot(0)
      p1 += plot(linspace(0, timeSteps, timeSteps), mean(average(::, *)).inner, colorcode=color(epsilon),name="epsilon ="+epsilon)
      p1.xlabel = "Steps"
      p1.ylabel = "Average"
      p1.title = "Epsilon Greedy"
      p1.legend=true
    }
  }
    def incrementalSimulation = {
      val timeSteps = 1000
      val f = Figure()
      val f1 = Figure()
      val epsilon=0.1
      for (stepSize <- Seq(0.1, 0.2, 0.3)) {
        val bandits = Array.fill(1000)(new Bandit(incrementalArm(epsilon, stepSize)))
        val (bestActions, average, _) = banditSimulation(1000, timeSteps, bandits)

        val p0 = f.subplot(0)
        p0 += plot(linspace(0, timeSteps, timeSteps), sum(bestActions(::, *)).inner, colorcode = color(stepSize), name="stepSize ="+stepSize)
        p0.xlabel = "Steps"
        p0.ylabel = "Best Action"
        p0.title = "Incremental"
        p0.legend=true

        val p1= f1.subplot(0)
        p1 += plot(linspace(0, timeSteps, timeSteps), mean(average(::, *)).inner, colorcode=color(stepSize),name="stepSize ="+stepSize)
        p1.xlabel = "Steps"
        p1.ylabel = "Average"
        p1.title = "Incremental"
        p1.legend=true
      }
  }
  def ucbSimulation = {
    val timeSteps = 1000
    val f = Figure()
    val epsilon=0.1
    val f1 = Figure()
    for (ucb <- Seq(1, 2, 3)) {
      val bandits = Array.fill(1000)(new Bandit(ucbArm(ucb)))
      val (bestActions, average, _) = banditSimulation(1000, timeSteps, bandits)
      val p0 = f.subplot(0)
      p0 += plot(linspace(0, timeSteps, timeSteps), sum(bestActions(::, *)).inner, colorcode = color(ucb), name="ucb ="+ucb)
      p0.xlabel = "Steps"
      p0.ylabel = "Best Actiosn"
      p0.title = "Upper-Confidence-Bound"
      p0.legend=true

      val p1= f1.subplot(0)
      p1 += plot(linspace(0, timeSteps, timeSteps), mean(average(::, *)).inner, colorcode=color(ucb),name="ucb ="+ucb)
      p1.xlabel = "Steps"
      p1.ylabel = "Average"
      p1.title = "Upper-Confidence-Bound"
      p1.legend=true
    }
  }
  def gradientSimulation = {
    val timeSteps = 4000
    val f = Figure()
    val epsilon=0.1
    val f1 = Figure()
    for (gradient <- Seq(.1, .2, .3)) {
      val bandits = Array.fill(1000)(new Bandit(gradientArm(gradient, 0, true)))
      val (bestActions, average, _) = banditSimulation(1000, timeSteps, bandits)
      val p0 = f.subplot(0)
      p0 += plot(linspace(0, timeSteps, timeSteps), sum(bestActions(::, *)).inner, colorcode = color(gradient), name="Gradient ="+gradient)
      p0.xlabel = "Steps"
      p0.ylabel = "Best Actions"
      p0.title = "Gradient"
      p0.legend=true

      val p1= f1.subplot(0)
      p1 += plot(linspace(0, timeSteps, timeSteps), mean(average(::, *)).inner, colorcode=color(gradient),name="Gradient ="+gradient)
      p1.xlabel = "Steps"
      p1.ylabel = "Average"
      p1.title = "Gradient"
      p1.legend=true
    }
  }
  def bayseanSimulation = {
    val timeSteps = 1000
    val f = Figure()
    val bandits = Array.fill(1000)(new Bandit(bayseanArm(trueReward=1)))
    val (bestActions, average, regrets) = banditSimulation(1000, timeSteps, bandits)
    val p0 = f.subplot(0)
    p0 += plot(linspace(0, timeSteps, timeSteps), regrets, colorcode="RED",name="Baysean")
    p0.xlabel = "Steps"
    p0.ylabel = "regrets"
    p0.title = "Baysean Regrets"

  }
  private def  color(epslon:Double):String = epslon match {
    case 0.1 | 1 => "RED"
    case 0.2 | 2=> "BLUE"
    case 0.3 | 3=> "YELLOW"
    case _ => "BLACK"
  }

 // epsilonGreedySimulation
//  incrementalSimulation
  //ucbSimulation
 // gradientSimulation
  bayseanSimulation


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
}
