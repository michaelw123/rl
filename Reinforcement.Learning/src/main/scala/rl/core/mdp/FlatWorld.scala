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

import breeze.linalg.{DenseMatrix, DenseVector, sum}
import breeze.numerics.abs
import rl.core.mdp.GridWorld.gridWorldAgent.{epoch, exitDelta, policyIteration, valueIteration}
import rl.utils.rounded

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
    case class Left(override val value:Int=0) extends flatWorldAction
    case class Right(override val value:Int=1) extends flatWorldAction
  }
  class flatWorldState(val id:Int, var value:Double) extends State[Int]

  object flatWorldAgent extends Agent[flatWorldAction, DenseVector, flatWorldState] {
    def observe[VF <: ValueFunction, P <: Policy[flatWorldState, flatWorldAction], E <: Environment[DenseVector, flatWorldState, flatWorldAction]](env: E, policy: P)(implicit vf: VF): DenseVector[flatWorldState] = {
      def observeOnce: DenseVector[flatWorldState] = {
        val newStates = env.stateSpace
        newStates.map(state => {
          val action = policy.bestAction(state)
          val vrp = env.rewards(state, action).map(x => (x._1, x._2, x._3 * policy.actionProb(state, action)))
          state.value = vf.value(state, vrp) - env.cost(state, action)
        })
        newStates
      }

      def observeAndUpdatePolicy = {
        val newStates = env.stateSpace
        newStates.map(state => {
          var values = Map[flatWorldAction, Double]()
          val actions = policy.applicableActions(state)
          if (!actions.isEmpty) {
            for (action <- actions) {
              val vrp = env.rewards(state, action).map(x => (x._1, x._2, x._3 * policy.actionProb(state, action)))
              values += (action -> (vf.value(state, vrp) - env.cost(state, action)))
            }
            policy.update(state, values.maxBy(_._2)._1)
            state.value = values.maxBy(_._2)._2
          }
        })
        env.update(newStates)
      }

      def loopingWithEpoch: Unit = {
        for (i <- 0 until epoch) {
          val newStates = observeOnce
          env.update(newStates)
          val r = newStates.map(a => rounded(1, a.value))
          println(s"Epoch $i: $r")
        }
      }

      @tailrec
      def loopingWithExitDelta: Unit = {
        val newStates = observeOnce
        val delta: Double = sum(abs(env.getCurrentStates.map(a => a.value) - newStates.map(b => b.value)))
        env.update(newStates)
        if (delta > exitDelta) {
          loopingWithExitDelta
        }
      }

      @tailrec
      def policyIterate: Unit = {
        val newStates = observeOnce
        val delta: Double = sum(abs(env.getCurrentStates.map(a => a.value) - newStates.map(b => b.value)))
        env.update(newStates)
        if (delta > exitDelta) {
          policyIterate
        } else {
          observeAndUpdatePolicy
          if (!policy.isStable) {
            policyIterate
          }
        }
      }

      @tailrec
      def valueIterate: Unit = {
        val oldStates = env.getCurrentStates
        observeAndUpdatePolicy
        val newState = env.getCurrentStates
        val delta: Double = sum(abs(env.getCurrentStates.map(a => a.value) - oldStates.map(b => b.value)))
        if (delta > exitDelta) {
          valueIterate
        }
      }

      (valueIteration, policyIteration, exitDelta, epoch) match {
        case (false, false, 0.0, _) => loopingWithEpoch
        case (false, false, _, _) => loopingWithExitDelta
        case (false, true, _, _) => policyIterate
        case (true, _, _, _) => valueIterate

      }
      env.getCurrentStates
    }
  }
}
