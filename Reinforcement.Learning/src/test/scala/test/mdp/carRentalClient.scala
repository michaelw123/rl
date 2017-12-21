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
import rl.core.mdp.Environment
import rl.core.mdp.FlatWorld.{flatWorldAction, flatWorldState}
import rl.core.mdp.GridWorld.gridWorldAction.{East, North, South, West}
import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldAgent, gridWorldPolicy, gridWorldState}
import rl.utils._


/**
  * Created by Michael Wang on 12/18/2017.
  *
  * This program is an application to solve Car Rental problem in Sunnton/Barto's book
  * "Reinforcement Learning: An Introduction (2nd Edition)" chapter 4, on the MDP framework developed in this project using gridWorld model
  */
object carRentalClient extends App {
  val X = 20
  val Y = 20

  val MAXREQUEST = 10
  val MAXRETURN = 5
  val MOVINGCOST=2.0
  val RENTINCOME = 10.0
  // expectation for rental requests in first location
  val RENTAL_REQUEST_FIRST_LOC = 3
  // expectation for rental requests in second location
  val RENTAL_REQUEST_SECOND_LOC = 4
  // expectation for # of cars returned in first location
  val   RETURNS_FIRST_LOC = 3
  // expectation for # of cars returned in second location
  val   RETURNS_SECOND_LOC = 2
  val MAXMOVE = 5

  implicit object carRentalEnv extends Environment[DenseMatrix, gridWorldState, gridWorldAction] {
    val stateSpace: DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X, Y) { (i, j) => new gridWorldState((i, j), 0) }
    val actionSpace: Seq[gridWorldAction] = Seq(new gridWorldAction {
      override val value: Int = -5
    }, new gridWorldAction {
      override val value: Int = -4
    },
      new gridWorldAction {
        override val value: Int = -3
      }, new gridWorldAction {
        override val value: Int = -2
      },
      new gridWorldAction {
        override val value: Int = -2
      }, new gridWorldAction {
        override val value: Int = 0
      },
      new gridWorldAction {
        override val value: Int = 1
      }, new gridWorldAction {
        override val value: Int = 2
      },
      new gridWorldAction {
        override val value: Int = 3
      }, new gridWorldAction {
        override val value: Int = 4
      },
      new gridWorldAction {
        override val value: Int = 5
      })

    def getStates = currentStates

    override def reward(state: gridWorldState, action: gridWorldAction): (gridWorldState, Double) = {
      val theCost = cost(state, action)
      var reward = 0.0
      for (firstLocationRequest <- 0 until scala.math.min(state.id._1,RENTAL_REQUEST_FIRST_LOC)  - action.value; secondLocationRequest <- 0 until scala.math.min(state.id._2 + action.value, RENTAL_REQUEST_SECOND_LOC)) {
        val probRequest = poisson(RENTAL_REQUEST_FIRST_LOC, firstLocationRequest) * poisson(RENTAL_REQUEST_SECOND_LOC, secondLocationRequest)
        reward += (firstLocationRequest + RENTAL_REQUEST_SECOND_LOC) * RENTINCOME * probRequest
      }
      (new gridWorldState(state.id, 0), reward - theCost)
    }

    override def transactionProb(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = {
      val diff1 = scala.math.abs(state.id._1 - action.value - nextState.id._1)
      val diff2 = scala.math.abs(nextState.id._2 + action.value - state.id._2)
      var reward:Double = 0.0
 //     println (state.id + " "+ action.value + " " + nextState.id)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._1)) {
        reward += poisson(RENTAL_REQUEST_FIRST_LOC, i) * poisson(RETURNS_FIRST_LOC, scala.math.abs(diff1 -i)) +i
      }
//      println("prob1="+prob)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._2)) {
        reward += poisson(RENTAL_REQUEST_SECOND_LOC, i) * poisson(RETURNS_SECOND_LOC, scala.math.abs(diff2 -i))
      }
     // println("prob = "+prob)
      reward
    }

    override def cost(state: gridWorldState, action: gridWorldAction): Double = action.value * MOVINGCOST

    override def reward(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = {

      action.value * RENTINCOME
    }

    override def cost(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = action.value * MOVINGCOST

    override def availableTransactions(state: gridWorldState): Seq[(gridWorldAction, gridWorldState)] =
      for (action <- availableActions(state); nextState <- stateSpace.toArray) yield (action, nextState)

    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = {
      val first2second = for (i <- 0 until scala.math.min(state.id._1, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = i
      }
      val second2first = for (i <- 0 until scala.math.min(state.id._2, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = (-1) * i
      }
      first2second ++ second2first
    }
  }


  implicit val policy:gridWorldPolicy = new gridWorldPolicy
  import rl.core.mdp.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)
  val result = gridWorldAgent.setEpoch(10)
    //.setExitDelta(0.001)
    .observe

  println(result.map(a => rounded(3, a.value)))
}
