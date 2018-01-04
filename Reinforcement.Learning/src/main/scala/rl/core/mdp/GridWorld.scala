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

import breeze.linalg.{DenseMatrix, DenseVector, ImmutableNumericOps, argmax, sum}
import breeze.numerics.abs
import rl.utils.rounded

import scala.annotation.tailrec
import rl.utils._
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
  
  class gridWorldState(val id:(Int, Int), var value:Double) extends State[(Int, Int)]

  object gridWorldAgent extends Agent[gridWorldAction, DenseMatrix, gridWorldState]{
    def observe[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[DenseMatrix,gridWorldState, gridWorldAction]](env:E, policy:P)(implicit vf:VF): DenseMatrix[gridWorldState] = {
      @tailrec
      def iterating:Unit = {
        val newStates = observeOnce
        val delta: Double = sum(abs(env.getCurrentStates.map(a => a.value) - newStates.map(b => b.value)))
        println(s"delta=$delta")
        env.update(newStates)
        if (delta > exitDelta) {
          iterating
        } else {
          observeAndUpdatePolicy
          if (!policy.isStable) {
            iterating
          }
        }
      }
      def looping = {
        for (i <- 0 until epoch) {
          val newStates = observeOnce
          env.update(newStates)
          val r = newStates.map(a => rounded(1, a.value))
          //println(s"Epoch $i: $r")
        }
      }
        def observeOnce: DenseMatrix[gridWorldState] = {

          val newStates = env.stateSpace
          newStates.map(state => {
            val action = policy.bestAction(state)
            val vrp = env.rewards(state, action).map(x => (x._1, x._2- env.cost(state, action), x._3 * policy.actionProb(state, action)))
            state.value=vf.value(state, vrp)
          })
          newStates
        }
      def observeAndUpdatePolicy = {
        val newStates = env.stateSpace
        newStates.map(state => {
          var values = Map[gridWorldAction, Double]()
          for (action <- policy.applicableActions(state)) {
            val vrp = env.rewards(state, action).map(x => (x._1, x._2 - env.cost(state, action), x._3 * policy.actionProb(state, action)))
            values += (action -> vf.value(state, vrp))
          }
          policy.update(state, values.maxBy(_._2)._1)
        })
      }

        def tmpFindValueByStateAction(state: gridWorldState, action: gridWorldAction) = {
          var returns:Double = - action.value * 2
          val ccStates = env.getCurrentStates
          var vrp = Seq[(Double, Double, Double)]()
          for (rentalRequestFirstLoc <- 0 until 11) {
            for (rentalRequestSecondLoc <- 0 until 11) {
              var numOfCarsFirstLoc = scala.math.min(state.id._1 - action.value, 20)
              var numOfCarsSecondLoc = scala.math.min(state.id._2 + action.value, 20)
              val realRentalFirstLoc = scala.math.min(numOfCarsFirstLoc, rentalRequestFirstLoc)
              val realRentalSecondLoc = scala.math.min(numOfCarsSecondLoc, rentalRequestSecondLoc)
              val reward:Double = (realRentalFirstLoc + realRentalSecondLoc) * 10
              numOfCarsFirstLoc -= realRentalFirstLoc
              numOfCarsSecondLoc -= realRentalSecondLoc

              val prob = poisson(3, rentalRequestFirstLoc) * poisson(4, rentalRequestSecondLoc)
              val returnedCarsFirstLoc = 3
              val returnedCarsSecondLoc = 2

              numOfCarsFirstLoc = scala.math.min(numOfCarsFirstLoc + returnedCarsFirstLoc, 20)
              numOfCarsSecondLoc = scala.math.min(numOfCarsSecondLoc + returnedCarsSecondLoc, 20)
              vrp = vrp :+ (ccStates(numOfCarsFirstLoc, numOfCarsSecondLoc).value, reward, prob)
            }
          }
          returns += vf.value(state, vrp)
          returns
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
