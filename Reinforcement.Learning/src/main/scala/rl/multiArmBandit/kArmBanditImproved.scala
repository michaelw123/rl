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
  case class averageGreedyArm() extends Arm
  case class incrementalArm(stepSize: Double) extends Arm
  case class ucbArm(ucb: Int) extends Arm
  case class gradientArm(stepSize: Double = 0) extends Arm
  case class initializationArm(initial: Double) extends Arm

  class Bandit[T](arm: T, k: Int)(implicit action: Action[T]) {
    val arms = List.fill[T](k)(arm)
    val trueQ = DenseVector.fill[Double](k)(math.random)
    val qEstimation = DenseVector.fill[Double](k)(0)
    val actionCount = Array.fill[Int](k)(0)
    var actionProb = DenseVector.zeros[Double](k)

    def getArm = action.getArm

    def play(atm:T) = action.play(arm:T)
  }


  trait Action[T] {
    def getArm: T
    def play[T](arm:T): Double
  }
  implicit object averageGreedyArmAction extends Action[averageGreedyArm] {
    def getArm:averageGreedyArm= {
      println("get averageGreedyArm")
      averageGreedyArm()
    }
    def play[averageGreedyArm](arm:averageGreedyArm) = {
      println("play averageGreedyArm")
      0.0
    }
  }
  implicit object incrementalArmAction extends Action[incrementalArm] {
    def getArm:incrementalArm = {
      println("get incrementalArm")
      incrementalArm(0.1)
    }
    def play[incrementalArm](arm:incrementalArm) = {
      println("play incrementalArm")
      0.0
    }
  }


  val bandit = new Bandit[averageGreedyArm](averageGreedyArm(), 10)
  val arm:averageGreedyArm = bandit.getArm
  bandit.play(arm)

  val bandit2 = new Bandit[incrementalArm](incrementalArm(0.1), 10)
  val arm2 = bandit2.getArm
  bandit2.play(arm2)
  val bandits = List.fill[Bandit[averageGreedyArm]](100)(new Bandit[averageGreedyArm](averageGreedyArm(), 10))
  //bandits.foreach(_.getArm)

}
