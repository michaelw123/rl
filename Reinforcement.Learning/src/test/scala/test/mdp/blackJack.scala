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
package test.mdp

import breeze.linalg.DenseMatrix
import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldAgent, gridWorldState}
import rl.core.mdp.{State, _}
import rl.core.mdp.MultiDimentionalWorld._

import scala.util.Random

/**
  * Created by wangmich on 01/24/2018.
  */
object blackJack extends App {
  val X = 11 *2
  val Y = 11 *2
  class blackJackState(val id:(Int, Int), var value:Double) extends State[(Int, Int)] {
    var playerTrajectory:Seq[blackJackAction] = Seq()
    var dealerTrajectory:Seq[blackJackAction] = Seq()
    def isHandSoft = id._1 < 11
    def isDealerSoft = id._1 < 11
    def dealerSum = id._2
    def sum = id._1
  }
  class blackJackAction
  object blackJackAction {
    sealed
    case object hit extends blackJackAction
    case object stay extends blackJackAction
  }
  implicit def grid2BlackJackState(s:gridWorldState):blackJackState = new blackJackState(s.id, s.value)
  implicit def grid2BlackJackAction(a:gridWorldAction):blackJackAction = new blackJackAction
  object blackJackEnv extends Environment[DenseMatrix, blackJackState, blackJackAction] {
    //X: sum of cards, 2 <= sum <= 21. keep drawing until sum is at least 11, so 11 <= sum <= 21.
    //Thus, i=0 is burst; i shows sum i  or i+10 (soft when i<=10)
    // Y: sum of dealer's showing card. j=0: burst, j shows sum j  and j+10 (soft when j<=10)
    override def stateSpace: DenseMatrix[blackJackState] = DenseMatrix.tabulate[blackJackState](X + 1, Y + 1) { (i, j) => new blackJackState((i, j), 0) }
    override val actionSpace: Seq[blackJackAction] = Seq(blackJackAction.hit, blackJackAction.stay)
//    override def reward(state: blackJackState, action: blackJackAction): (blackJackState, Double) = {//reward through MCMC
    }
    object blackJackPolicy extends Policy[blackJackState, blackJackAction] {
      var policy = DenseMatrix.tabulate[blackJackAction] (X+1, Y+1) { (i, j) => {
        val state: blackJackState = new blackJackState((i, j), 0)
        state.sum match {
          case 10 | 11 | 20 | 21 => blackJackAction.stay
          case _ => blackJackAction.hit
        }
       }
      }
      override  def optimalPolicy(state:blackJackState):blackJackAction = policy(state.id)
      override  def applicableActions(state: blackJackState): Seq[blackJackAction] = Seq(blackJackAction.hit, blackJackAction.stay)
      override  def actionProb(state:blackJackState, action:blackJackAction):Double = 1.0
    }
    val result = gridWorldAgent.setEpoch(50)
      .setExitDelta(0.1)
      .setPolicyIteration(true)
      .setValueIteration(true)
      .observe(blackJackEnv, blackJackPolicy)
}
