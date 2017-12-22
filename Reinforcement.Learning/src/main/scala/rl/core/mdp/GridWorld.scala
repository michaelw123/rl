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

import breeze.linalg.{DenseMatrix, ImmutableNumericOps, sum}
import breeze.numerics.abs
import rl.utils.rounded

import scala.annotation.tailrec
/**
  * Created by Michael Wang on 2017-12-09.
  */
object GridWorld {
  trait gridWorldAction extends Action
  object gridWorldAction {
    sealed
    case class North(override val value:Int=0) extends gridWorldAction
    case class East(override val value:Int=1) extends gridWorldAction
    case class South(override val value:Int=2) extends gridWorldAction
    case class West(override val value:Int=3) extends gridWorldAction
  }

  class gridWorldPolicy extends Policy[gridWorldState, gridWorldAction] {
    import gridWorldAction._
    //override def availableActions(state: gridWorldState): Seq[gridWorldAction] = Seq(new North, new East, new South, new West)

  }

  class gridWorldState(val id:(Int, Int), var value:Double) extends State[(Int, Int)]

  object gridWorldAgent extends Agent[gridWorldAction, DenseMatrix, gridWorldState]{
    def observe[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[DenseMatrix,gridWorldState, gridWorldAction]](implicit vf:VF, policy:P, env:E): DenseMatrix[gridWorldState] = {
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
            val actionState = env.availableTransactions(state)
            var vrp = Seq[(Double, Double, Double)]()
            for ((action, nextState) <- actionState) {
//              val actionProb = env.transactionProb(state, action, nextState)
//              val reward = env.reward(state, action, nextState)
              val (actionProb, reward ) = env.transactionRewardProb(state, action, nextState)
              vrp = vrp :+ (nextState.value, reward - env.cost(state, action, nextState), actionProb)
            }
            state.value = vf.value(state, vrp)
          })
          env.update(newStates)
          val r = newStates.map(a => rounded(3, a.value))
         println(s"Epoch $i: $r")
        }
      }
      def observeOnce:DenseMatrix[gridWorldState] = {
        val newStates = env.stateSpace
        newStates.map(state => {
          val actionState = env.availableTransactions(state)
          var vrp = Seq[(Double, Double, Double)]()
          for ((action, nextState) <- actionState) {
            val actionProb = env.transactionProb(state, action, nextState)
            val reward = env.transactionProb(state, action, nextState)
            vrp = vrp :+ (nextState.value, reward - env.cost(state, action, nextState), actionProb)
          }
          state.value = vf.value(state, vrp)
        })
        newStates
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
