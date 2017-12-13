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
package rl.experiment.mdp.core

import breeze.linalg.DenseMatrix

import rl.experiment.mdp.core.State
/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */

object ValueFunctions {

  implicit object Bellman extends ValueFunction {
    private var discount = 0.0

    override def setDiscount(value: Double): this.type = {
      discount = value
      this
    }

    override def getDiscount = discount

    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      (statevalue + prob * (reward + discount * nextStateValue))
    }
    override def value[ID, P <:Policy[State[ID], Action]] (state:State[ID])(implicit policy:P):Double = {
      val actions = policy.availableActions(state)
      actions.foldLeft(state.value)((a,b) => {
        val (nextState, reward) = policy.reward(state, b)
        val actionProb = policy.getActionProb(b)
        //value(a, resultState(nextState.id).value, reward, actionProb)
        0.0
      })
    }
    override def value[ID, P<:Policy[State[ID], Action]](state:State[ID])(implicit policy:P, env:Environment[State[ID]]):Unit = {
      val result = env.result
      val newStates = env.stateSpace
      newStates.map(state => state.value = {
        val actions = policy.availableActions(state)
        actions.foldLeft(state.value)((a, b) => {
          val (nextState, reward) = policy.reward(state, b)
          val actionProb = policy.getActionProb(b)
          value(a, nextState.value, reward, actionProb)
        })
      })
    }
  }

  implicit object optimalValueIteration extends ValueFunction {
    private var discount = 0.0

    override def setDiscount(value: Double): this.type = {
      discount = value
      this
    }

    override def getDiscount = discount

    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      reward + discount * nextStateValue
    }
    override def value[S, P] (state:S)(implicit policy:P):Double = {
      policy.availableActions
    }
  }

}


