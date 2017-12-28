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
import rl.core.mdp.GridWorld._
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
  val POISSONUPBOUND=11
  val MAX_CARS=20

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
        override val value: Int = -1
      },
      new gridWorldAction {
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

    override def reward(state: gridWorldState, action: gridWorldAction): (gridWorldState, Double) = {
      val theCost = cost(state, action)
      var reward = 0.0
      for (firstLocationRequest <- 0 until scala.math.min(state.id._1,RENTAL_REQUEST_FIRST_LOC)  - action.value; secondLocationRequest <- 0 until scala.math.min(state.id._2 + action.value, RENTAL_REQUEST_SECOND_LOC)) {
        val probRequest = poisson(RENTAL_REQUEST_FIRST_LOC, firstLocationRequest) * poisson(RENTAL_REQUEST_SECOND_LOC, secondLocationRequest)
        reward += (firstLocationRequest + RENTAL_REQUEST_SECOND_LOC) * RENTINCOME * probRequest
      }
      //(getStates(r._1),  reward - theCost)
      (new gridWorldState(state.id, 0), reward - theCost)
    }

    override def transactionProb(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = {
      val diff1 = scala.math.abs(state.id._1 - action.value - nextState.id._1)
      val diff2 = scala.math.abs(nextState.id._2 + action.value - state.id._2)
      var prob1 = 0.0
      var prob2 = 0.0
 //     println (state.id + " "+ action.value + " " + nextState.id)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._1)) {
        prob1 += poisson(RENTAL_REQUEST_FIRST_LOC, i) * poisson(RETURNS_FIRST_LOC, scala.math.abs(diff1 -i))
      }
//      println("prob1="+prob)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._2)) {
        prob2 += poisson(RENTAL_REQUEST_SECOND_LOC, i) * poisson(RETURNS_SECOND_LOC, scala.math.abs(diff2 -i))
      }
     // println("prob = "+prob)
      prob1 * prob2
    }

    override def cost(state: gridWorldState, action: gridWorldAction): Double = scala.math.abs(action.value * MOVINGCOST)

    override def reward(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = {
      val diff1 = scala.math.abs(state.id._1 - action.value - nextState.id._1)
      val diff2 = scala.math.abs(state.id._2 + action.value - nextState.id._2)
      var reward:Double = 0.0
      //     println (state.id + " "+ action.value + " " + nextState.id)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._1)) {
        reward += poisson(RENTAL_REQUEST_FIRST_LOC, i) * poisson(RETURNS_FIRST_LOC, scala.math.abs(diff1 -i))*i*RENTINCOME
      }
      //      println("prob1="+prob)
      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._2)) {
        reward += poisson(RENTAL_REQUEST_SECOND_LOC, i) * poisson(RETURNS_SECOND_LOC, scala.math.abs(diff2 -i))*i*RENTINCOME
      }
      // println("prob = "+prob)
      reward
    }
     def transactionRewardProb1(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double) = {
      val diff1 = {
        if (action.value >0) state.id._1 - action.value - nextState.id._1
        else  state.id._1  - nextState.id._1
      }
      val diff2 = {
        if (action.value<0) state.id._2 + action.value - nextState.id._2
        else  state.id._2  - nextState.id._2
      }
      var reward = 0.0
      var prob1 = 0.0
      var prob2 = 0.0
      //     println (state.id + " "+ action.value + " " + nextState.id)
      val r1 = {
        if (action.value >0) state.id._1 - action.value
        else state.id._1
      }
      for (i <- 1 until scala.math.min(MAXREQUEST, r1)) {
        val k = scala.math.abs(i-diff1)
        if (k<POISSONUPBOUND) {
          val tmp = poisson(RENTAL_REQUEST_FIRST_LOC, i) * poisson(RETURNS_FIRST_LOC, k)
          prob1 += tmp
          reward += tmp * i
        }
      }
      val r2 = {
        if (action.value <0) state.id._2 + action.value
        else state.id._2
      }
      //      println("prob1="+prob)
      for (i <- 1 until scala.math.min(MAXREQUEST, r2)) {
        val k = scala.math.abs(i-diff2)
        if (k<POISSONUPBOUND) {
          val tmp = poisson(RENTAL_REQUEST_SECOND_LOC, i) * poisson(RETURNS_SECOND_LOC, k)
          prob2 += tmp
          reward += tmp * i
        }

      }
      val v = action.value
      val s1=state.id._1
      val s2=state.id._2
      val ns1=nextState.id._1
      val ns2=nextState.id._2
//      if (state.id == (18, 17) && action.value == -3) {
//        println(s"diff1=$diff1, diff2=$diff2, state.id._1=$s1, state.id._2=$s2, nextState.id._1=$ns1, nextState.id._2=$ns2, action=$v, r1=$r1, r2=$r2, reward=$reward")
//      }
//      var prob = 0.0
//      for (i <- 0 until scala.math.min(MAXREQUEST, state.id._1-action.value)) {
//        for (j <- 0 until scala.math.min(MAXREQUEST, state.id._2+action.value)) {
//          val tmp1 = poisson(RENTAL_REQUEST_FIRST_LOC, i) * poisson(RETURNS_FIRST_LOC, scala.math.abs(diff1 - i))
//          val tmp2 = poisson(RENTAL_REQUEST_SECOND_LOC, j) * poisson(RETURNS_SECOND_LOC, scala.math.abs(diff2 - j))
//          prob += tmp1 * tmp2
//          reward += (i + j)*tmp1*tmp2
//        }
//      }
      val prob = prob1 * prob2
      //println(s"(prob, reward)=($prob, $reward)")
      (prob, reward*RENTINCOME)
    }
    override def transactionRewardProb(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double) = {
      val numberOfReq1 = scala.math.min(scala.math.min(state.id._1-action.value,MAX_CARS), POISSONUPBOUND)
      val numberOfReq2 = scala.math.min(scala.math.min(state.id._2+action.value, MAX_CARS),POISSONUPBOUND)

      var req1 = state.id._1 -action.value -nextState.id._1 + RETURNS_FIRST_LOC
      var req2 = state.id._2 +action.value -nextState.id._2 + RETURNS_SECOND_LOC
      if (req1<0 || req1 > numberOfReq1) {
        req1=0
      }
      if (req2<0 || req2 > numberOfReq2) {
        req2=0
      }
      val prob = poisson(RENTAL_REQUEST_FIRST_LOC, req1) * poisson(RENTAL_REQUEST_SECOND_LOC, req2)
      val reward = (req1+req2)*RENTINCOME
      //println(prob, reward)
      (prob, (req1+req2)*RENTINCOME)

    }
    override def cost(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = scala.math.abs(action.value * MOVINGCOST)

    override def availableTransactions(state: gridWorldState): Seq[(gridWorldAction, gridWorldState)] = {
      val actions = availableActions(state)
      val transactions = getCurrentStates.toArray
      for (action <- actions; nextState <- transactions) yield (action, nextState)
    }

    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = {
      val first2second = for (i <- 0 until scala.math.min(state.id._1, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = i
      }
      val second2first = for (i <- 1 until scala.math.min(state.id._2, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = (-1) * i
      }
      first2second ++ second2first
    }
    override def getCurrentStates:DenseMatrix[gridWorldState] = {
      if (!currentStates.isDefined) currentStates=Option(stateSpace)
      currentStates.get
    }
  }


  implicit val policy:gridWorldPolicy = new gridWorldPolicy
  import rl.core.mdp.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)
//  import rl.core.mdp.ValueFunctions.qlearning
//  qlearning.setDiscount(0.9).setLearningRate(0.5)
  val result = gridWorldAgent.setEpoch(20)
    //.setExitDelta(0.001)
    .observe

  println(result.map(a => rounded(3, a.value)))


}
