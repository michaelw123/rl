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

package rl.kArmBandit

import breeze.linalg._
import breeze.numerics.{exp, log, sqrt}
import breeze.stats._
import breeze.plot._
import breeze.stats.distributions.{Binomial, Rand, RandBasis, ThreadLocalRandomGenerator}
import org.apache.commons.math3.random.MersenneTwister
import shapeless.T

import scala.annotation.tailrec

/*
  * Created by Michael Wang on 10/30/2017.
  */
trait Arm
sealed
case class averageGreedyArm() extends Arm
case class incrementalArm(stepSize:Double) extends Arm
case class ucbArm(ucb:Int) extends Arm
case class gradientArm(stepSize:Double = 0) extends Arm
case class initializationArm(initial:Double ) extends Arm

trait Action[T <: Arm] {
  def getArm:T
  def playArm[T]:Double
}
object Action {
  implicit def getArm[averageGreedyArm] = {

  }
}


class Bandit[T <: Arm](k:Int) {
  val arms = DenseVector.zeros[Arm](k)
  val trueQ = DenseVector.fill[Double](k)(math.random)
  val qEstimation = DenseVector.fill[Double](k)
  val actionCount = Array.fill[Int](k)(0)
  var actionProb = DenseVector.zeros[Double](k)
}