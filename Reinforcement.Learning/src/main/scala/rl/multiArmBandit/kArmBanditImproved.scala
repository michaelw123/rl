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
import breeze.stats._
import breeze.plot._
import breeze.stats.distributions.{Binomial, Rand, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister


import scala.annotation.tailrec

/*
  * Created by Michael Wang on 10/30/2017.
  */

object multiArmBandit extends App {
  trait Arm
  case class averageGreedyArm(epsilon:Double) extends Arm
  case class incrementalArm(stepSize: Double) extends Arm
  case class ucbArm(ucb: Int) extends Arm
  case class gradientArm(stepSize: Double = 0) extends Arm
  case class initializationArm(initial: Double) extends Arm

  trait Bandit[T]{//(implicit action: Action[T]) {
    val k:Int = 10
    var time=0
    val trueQ = DenseVector.fill[Double](k)(math.random)
    val qEstimation = DenseVector.fill[Double](k)(0)
    val actionCount = Array.fill[Int](k)(0)
    var actionProb = DenseVector.zeros[Double](k)

  }
   class averageGreedyBandit(epsilon:Double) extends Bandit[averageGreedyArm] with Action{
    val arms = List.fill[averageGreedyArm](k)(averageGreedyArm(epsilon))

    def getArm:Int = {
      System.out.println("getArm averageGreedyBandit")
      if (Binomial(1, epsilon).draw == 1) scala.util.Random.nextInt(k) else  argmax(qEstimation)
    }
    def play(arm:Int):Double ={
      System.out.println("play averageGreedyBandit")
      val reward = trueQ.valueAt(arm) + math.random
      time += 1
      actionCount(arm) = actionCount(arm)+1
      qEstimation(arm) += 1.0 / actionCount(arm) * (reward - qEstimation(arm))
      reward
    }
  }

  trait Action {
    def getArm: Int
    def play(arm:Int): Double
  }

  val bandit =  new averageGreedyBandit(0.1)
  val arm:Int = bandit.getArm
  bandit.play(arm)

//  val bandit2 = new Bandit[incrementalArm](incrementalArm(0.1), 10)
//  val arm2 = bandit2.getArm
//  bandit2.play(arm2)
//  val bandits = List.fill[Bandit[averageGreedyArm]](100)(new Bandit[averageGreedyArm](averageGreedyArm(0.1), 10))
//  //bandits.foreach(_.getArm)

}
