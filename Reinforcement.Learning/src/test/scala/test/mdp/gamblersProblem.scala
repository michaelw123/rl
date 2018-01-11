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
import breeze.linalg.{DenseMatrix, DenseVector}
import rl.core.mdp.{Environment, Policy}
import rl.core.mdp.FlatWorld.{flatWorldAction, flatWorldState}
import rl.core.mdp.FlatWorld.flatWorldAgent

/**
  * Created by wangmich on 01/11/2018.
  */
object gamblersProblem extends App{
  val GOAL=100
  val headProb = 0.4
  object gamblersProblemEnv extends Environment[DenseVector, flatWorldState, flatWorldAction] {
    def stateSpace: DenseVector[flatWorldState] = DenseVector.tabulate[flatWorldState](GOAL+1) { x => new flatWorldState((x), 0) }
    val actionSpace: Seq[flatWorldAction] = Seq.tabulate[flatWorldAction](GOAL+1)(x => new flatWorldAction {override val value: Int = x})
    override def rewards(state:flatWorldState, action:flatWorldAction):Seq[(Double, Double, Double)] = {
      val ccStates = getCurrentStates
      var vrp = Seq[(Double, Double, Double)] ()
      val actions = gamblersProblemPolicy.applicableActions(state)
      vrp :+ (state.id + action.value,action.value, headProb)
      vrp :+ (state.id - action.value,-action.value, (1-headProb))
      vrp

    }
  }
  object gamblersProblemPolicy extends Policy[flatWorldState, flatWorldAction] {
    var policyCopy = DenseVector.tabulate[flatWorldAction](GOAL + 1) { x => new flatWorldAction {
      override val value: Int = x
    }
    }
    var policy = DenseVector.tabulate[flatWorldAction](GOAL + 1) { x => new flatWorldAction {
      override val value: Int = x
    }
    }

    override def actionProb(state: flatWorldState, action: flatWorldAction): Double = 0.4

    override def bestAction(state: flatWorldState): flatWorldAction = policy(state.id)

    override def applicableActions(state: flatWorldState): Seq[flatWorldAction] = {
      val actions = Seq.tabulate[flatWorldAction](state.id + 1)(x => new flatWorldAction {
        override val value: Int = x
      })
      actions
    }
  }

  import rl.core.mdp.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)
  val result = flatWorldAgent.setEpoch(10)
    .setExitDelta(0.1)
    .setPolicyIteration(true)
    .setValueIteration(true)
    .observe(gamblersProblemEnv, gamblersProblemPolicy)
}
