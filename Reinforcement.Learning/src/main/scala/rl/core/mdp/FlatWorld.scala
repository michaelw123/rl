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
package rl.core.mdp

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.abs

import scala.annotation.tailrec

/**
  * Created by Michael Wang on 2017-12-17.
  *
  * FlatWorld models the grid in a scalar structure, using DenseVector instead of DenseMatrix.
  */
object FlatWorld {
  trait flatWorldAction extends Action
  object flatWorldAction {
    case object North extends flatWorldAction
    case object East extends flatWorldAction
    case object South extends flatWorldAction
    case object West extends flatWorldAction
  }

  class flatWorldPolicy extends Policy[flatWorldState, flatWorldAction] {
    import flatWorldAction._
    override def reward(state:flatWorldState, action:flatWorldAction):(flatWorldState, Double) = ???
    override def availableActions(state: flatWorldState): Seq[flatWorldAction] = Seq(North, East, South, West)
    override def getActionProb(state:flatWorldState,  action:flatWorldAction):Double = 0.25
    override def cost(state:flatWorldState, action:flatWorldAction):Double = 0.0
  }

  class flatWorldState(val id:Int, var value:Double) extends State[Int]

  object flatWorldAgent extends Agent[flatWorldAction, DenseVector, flatWorldState]{
    def observe[VF <: ValueFunction, P <: Policy[flatWorldState, flatWorldAction], E <: Environment[DenseVector, flatWorldState]](implicit vf:VF, policy:P, env:E): DenseVector[flatWorldState] = {
      @tailrec
      def iterating:Unit = {
        val newStates = observeOnce
        val x: Double = sum(abs(env.currentStates.map(a => a.value) - newStates.map(b => b.value)))
        env.update(newStates)
        if (x > exitValue) {
          iterating
        }
      }
      def looping = {
        for (i <- 0 until epoch) {
          val newStates = env.stateSpace

          newStates.map(state => {
            val actions = policy.availableActions(state)
            val vrp = for (action <- actions;
                           (nextState, reward) = policy.reward(state, action);
                           actionProb = policy.getActionProb(state, action)
            ) yield (nextState.value, reward - policy.cost(state, action), actionProb)
            state.value = vf.value(state, vrp)
          })
          env.update(newStates)
        }
      }
      def observeOnce:DenseVector[flatWorldState] = {
        val newStates = env.stateSpace

        newStates.map(state => {
          val actions = policy.availableActions(state)
          val vrp = for (action <- actions;
                         (nextState, reward) = policy.reward(state, action);
                         actionProb = policy.getActionProb(state, action)
          ) yield (nextState.value, reward - policy.cost(state, action), actionProb)
          state.value = vf.value(state, vrp)
        })
        newStates
      }
      exitValue match {
        case 0.0 => looping
        case _ => iterating
      }
      env.currentStates
    }

    private var epoch = 1
    def setEpoch(value:Int) : this.type ={
      epoch = value
      this
    }
    private var exitValue=0.0
    def setExitValue(value:Double) = {
      exitValue = value
      this
    }
  }
}
