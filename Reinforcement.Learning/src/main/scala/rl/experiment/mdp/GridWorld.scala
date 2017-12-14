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
package rl.experiment.mdp

import breeze.linalg.DenseMatrix
import rl.experiment.mdp.core.{Environment, _}
/**
  * Created by Michael Wang on 2017-12-09.
  */
object GridWorld {
  trait gridWorldAction extends Action
  object gridWorldAction {
    case object North extends gridWorldAction
    case object East extends gridWorldAction
    case object South extends gridWorldAction
    case object West extends gridWorldAction
  }
  
  class gridWorldPolicy extends Policy[gridWorldState, gridWorldAction] {
    import gridWorldAction._
    override def reward(state:gridWorldState, action:gridWorldAction):(gridWorldState, Double) = ???
    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = Seq(North, East, South, West)
    override def getActionProb(action:gridWorldAction):Double = 0.25
  }

  class gridWorldState(val id:(Int, Int), var value:Double) extends State[(Int, Int)]

  object gridWorldAgent extends Agent[gridWorldAction, gridWorldState] {
    def observe[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[gridWorldState]](implicit vf:VF, policy:P, env:E): DenseMatrix[gridWorldState] = {
      for (i <- 0 until epoch) {
        val newStates = env.stateSpace

        newStates.map(state => {
          val actions = policy.availableActions(state)
          val vrp = for (action <- actions;
             (nextState, reward) = policy.reward(state, action);
             actionProb = policy.getActionProb(action)
            //vvrp :+ (state.value, nextState.value, reward, actionProb)
          ) yield (nextState.value, reward, actionProb)
          state.value = vf.value(state, vrp)
        })
        env.setResult(newStates)

      }
      env.result
    }
    private var epoch = 10
    def setEpoch(value:Int) : this.type ={
      epoch = value
      this
    }
  }


}
