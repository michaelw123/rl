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
    sealed
    case class North(override val value:Int=0) extends flatWorldAction
    case class East(override val value:Int=1) extends flatWorldAction
    case class South(override val value:Int=2) extends flatWorldAction
    case class West(override val value:Int=3) extends flatWorldAction
  }
  class flatWorldState(val id:Int, var value:Double) extends State[Int]

  object flatWorldAgent extends Agent[flatWorldAction, DenseVector, flatWorldState]{
    def observe[VF <: ValueFunction, P <: Policy[flatWorldState, flatWorldAction], E <: Environment[DenseVector, flatWorldState, flatWorldAction]](env:E, policy:P)(implicit vf:VF): DenseVector[flatWorldState] = {
      @tailrec
      def iterating:Unit = {
        val newStates = observeOnce
        val x: Double = sum(abs(env.getCurrentStates.map(a => a.value) - newStates.map(b => b.value)))
        env.update(newStates)
        if (x > exitDelta) {
          iterating
        }
      }
      def looping = {
        for (i <- 0 until epoch) {
          val newStates = env.stateSpace
          newStates.map(state => {
            val actions = policy.availableActions(state)
            val vrp = for (action <- actions;
                           (nextState, reward) = env.reward(state, action);
                           actionProb = env.transitionProb(state, action, nextState)
            ) yield (nextState.value, reward - env.cost(state, action), actionProb)
            state.value = vf.value(state, vrp)
          })
          env.update(newStates)
        }
      }
      def observeOnce:DenseVector[flatWorldState] = {
        val newStates = env.stateSpace
        newStates.map(state => {
          val actionState = env.availableTransitions(state)
          var vrp = Seq[(Double, Double, Double)]()
          for ((action, nextState) <- actionState) {
            val actionProb = env.transitionProb(state, action, nextState)
            val reward = env.reward(state, action, nextState)
            vrp = vrp :+ (nextState.value, reward - env.cost(state, action, nextState), actionProb)
          }
          state.value = vf.value(state, vrp)
        })
        newStates
//        val newStates = env.stateSpace
//
//        newStates.map(state => {
//          val actions = env.availableActions(state)
//          val vrp = for (action <- actions;
//                         (nextState, reward) = env.reward(state, action);
//                         actionProb = env.transactionProb(state, action, nextState)
//          ) yield (nextState.value, reward - env.cost(state, action), actionProb)
//          state.value = vf.value(state, vrp)
//        })
//        newStates
      }

      exitDelta match {
        case 0.0 => looping
        case _ => iterating
      }
      env.getCurrentStates
    }

    private var epoch = 1
    def setEpoch(value:Int) : this.type ={
      epoch = value
      this
    }
    private var exitDelta=0.0
    def setExitDelta(value:Double) = {
      exitDelta = value
      this
    }
  }
}
