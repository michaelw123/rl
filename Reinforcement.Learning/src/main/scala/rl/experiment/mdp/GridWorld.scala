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
    //private var env:Environment[gridWorldAction, gridWorldState, DenseMatrix[gridWorldState]] = ???
    //     def setEnvironment(value: Environment[gridWorldAction, gridWorldState, DenseMatrix[gridWorldState]]):this.type = {
    //      env = value
    //      this
    //    }
      override def observe[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[ gridWorldState]] (implicit vf:VF, policy:P, env:E): DenseMatrix[gridWorldState] = {
           env.setResult(env.stateSpace)
          var resultState:DenseMatrix[gridWorldState] =env.result
          for (i <- 0 until epoch) {
            val newStates = env.stateSpace
            newStates.map(state => {
              state.value = policy.availableActions(state)
                .foldLeft(state.value)((a, b) => {
                  val (nextState, reward) = policy.reward(state, b)
                  val actionProb = policy.getActionProb(b)
                  vf.value(a, nextState.value, reward, actionProb)
                })
            })
            env.setResult(newStates)
          }
      env.result
    }
     def observe1[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[gridWorldState]] (implicit vf:VF, policy:P, env:E): DenseMatrix[gridWorldState] = {
       env.setResult(env.stateSpace)
       var resultState:DenseMatrix[gridWorldState] =env.stateSpace
      for (i <- 0 until epoch) {
        resultState.map(state => vf.value(state)(policy, env))
        env.setResult(resultState)
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
