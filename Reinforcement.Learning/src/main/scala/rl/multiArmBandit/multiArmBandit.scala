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

package rl.multiArmBandit

import breeze.linalg._
import breeze.numerics.{exp, log, sqrt}
import breeze.plot._
import breeze.stats.distributions.{Binomial, Rand, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister

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
          case true =>  bandit.averageReward
          case false => 0
        }
        bandit.qEstimation += (oneHot - bandit.actionProb) * bandit.arm.stepSize * (reward - bandit.baseline)
        reward
      }
    }

  }
  trait Arm
  case class averageGreedyArm(epsilon:Double) extends Arm
  case class incrementalArm(epsilon:Double, stepSize: Double) extends Arm
  case class ucbArm(ucb: Int) extends Arm
  case class gradientArm(stepSize: Double = 0, trueReward:Double =0, gradientaseline:Boolean=false, baseline:Double = 0) extends Arm

  class Bandit[T](anArm:T) {
    val k:Int = 10
    var time=0
    val trueQ = DenseVector.fill[Double](k)(math.random)
    val qEstimation = DenseVector.fill[Double](k)(0)
    val actionCount = Array.fill[Int](k)(0)
    var actionProb = DenseVector.zeros[Double](k)
    var averageReward:Double = 0
    var baseline:Double = 0
    def arm:T = anArm
  }
  def banditSimulation[T](n:Int, time:Int, bandits:Array[Bandit[T]])(implicit algo:Algorithm[T]) = {
    val bestActionCounts = DenseMatrix.zeros[Double] (bandits.length, time)
    val averageRewards = DenseMatrix.zeros[Double] (bandits.length, time)
    for (i <- 0 until n; t <- 1 until time ) {
      val bandit = bandits(i)
      val arm:Int = algo.getArm(bandit)
      val reward = algo.play(bandit, arm)
      averageRewards(i, t) += reward
      if (arm == algo.bestAction(bandit)) bestActionCounts(i, t) += 1
    }
    (bestActionCounts, averageRewards.map(_/bandits.length) )
  }

  def averageGreedySimulation = {
    val time = 1000
    val f = Figure()
    for (epsilon <- Seq(0.1, 0.2, 0.3)) {
      val bandits = Array.fill(1000)(new Bandit(averageGreedyArm(epsilon)))
      val time = 1000
      val (bestActions, average) = banditSimulation(1000, time, bandits)

      val p0 = f.subplot(0)
      p0 += plot(linspace(0, time, time), sum(bestActions(::, *)).inner, colorcode = color(epsilon))
      p0.xlabel = "Steps"
      p0.ylabel = "Best Action"
      p0.title = "epsilon =" + epsilon
      //    p0 += plot(linspace(0, time, time), mean(average(::, *)).inner, colorcode=color(epsilon))
      //    p0.xlabel = "Steps"
      //    p0.ylabel = "Average"
      //    p0.title = "epsilon =" +epsilon
    }
  }
    def incrementalSimulation = {
      val time = 1000
      val f = Figure()
      val epsilon=0.1
      for (stepSize <- Seq(0.1, 0.2, 0.3)) {
        val bandits = Array.fill(1000)(new Bandit(incrementalArm(epsilon, stepSize)))
        val time = 1000
        val (bestActions, average) = banditSimulation(1000, time, bandits)

        val p0 = f.subplot(0)
        p0 += plot(linspace(0, time, time), sum(bestActions(::, *)).inner, colorcode = color(epsilon))
        p0.xlabel = "Steps"
        p0.ylabel = "Best Action"
        p0.title = "epsilon =" + epsilon
      }
  }
  def ucbSimulation = {
    val time = 1000
    val f = Figure()
    val epsilon=0.1
    for (ucb <- Seq(1, 2, 3)) {
      val bandits = Array.fill(1000)(new Bandit(ucbArm(ucb)))
      val time = 1000
      val (bestActions, average) = banditSimulation(1000, time, bandits)
      val p0 = f.subplot(0)
      p0 += plot(linspace(0, time, time), sum(bestActions(::, *)).inner, colorcode = color(ucb))
      p0.xlabel = "Steps"
      p0.ylabel = "Best Action"
      p0.title = "UCB =" + ucb
    }
  }
  def gradientSimulation = {
    val time = 1000
    val f = Figure()
    val epsilon=0.1
    for (gradient <- Seq(.1, .2, .3)) {
      val bandits = Array.fill(1000)(new Bandit(gradientArm(gradient, 4, true)))
      val time = 1000
      val (bestActions, average) = banditSimulation(1000, time, bandits)
      val p0 = f.subplot(0)
      p0 += plot(linspace(0, time, time), sum(bestActions(::, *)).inner, colorcode = color(gradient))
      p0.xlabel = "Steps"
      p0.ylabel = "Best Action"
      p0.title = "Gragient =" + gradient
    }
  }
  private def  color(epslon:Double):String = epslon match {
    case 0.1 | 1 => "RED"
    case 0.2 | 2=> "BLUE"
    case 0.3 | 3=> "YELLOW"
    case _ => "BLACK"
  }

//  averageGreedySimulation
//  incrementalSimulation
//  ucbSimulation
  gradientSimulation


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
