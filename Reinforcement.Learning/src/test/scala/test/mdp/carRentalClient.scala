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
import rl.core.mdp.{Environment, Policy}
import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldAgent, gridWorldState}
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

   object carRentalEnv extends Environment[DenseMatrix, gridWorldState, gridWorldAction] {
    def stateSpace: DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X+1, Y+1) { (i, j) => new gridWorldState((i, j), 0) }
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
    override def rewards(state:gridWorldState, action:gridWorldAction):Seq[(Double, Double, Double)] = {
      val ccStates = getCurrentStates
      var vrp = Seq[(Double, Double, Double)] ()
      for (rentalRequestFirstLoc <- 0 until POISSONUPBOUND) {
        for (rentalRequestSecondLoc <- 0 until POISSONUPBOUND) {
          var numOfCarsFirstLoc = scala.math.min(state.id._1 - action.value, MAX_CARS)
          var numOfCarsSecondLoc = scala.math.min(state.id._2 + action.value, MAX_CARS)
          val realRentalFirstLoc = scala.math.min(numOfCarsFirstLoc, rentalRequestFirstLoc)
          val realRentalSecondLoc = scala.math.min(numOfCarsSecondLoc, rentalRequestSecondLoc)
          val prob = poisson(RENTAL_REQUEST_FIRST_LOC, rentalRequestFirstLoc) * poisson(RENTAL_REQUEST_SECOND_LOC, rentalRequestSecondLoc)
          val returnedCarsFirstLoc = RETURNS_FIRST_LOC
          val returnedCarsSecondLoc = RETURNS_SECOND_LOC
          val reward:Double = (realRentalFirstLoc + realRentalSecondLoc) * RENTINCOME
          numOfCarsFirstLoc = scala.math.min(numOfCarsFirstLoc + returnedCarsFirstLoc, MAX_CARS)
          numOfCarsSecondLoc = scala.math.min(numOfCarsSecondLoc + returnedCarsSecondLoc, MAX_CARS)
          vrp = vrp :+ (ccStates(numOfCarsFirstLoc, numOfCarsSecondLoc).value, reward, prob)
        }
      }
      vrp
    }

    override def transitionProb(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = {
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

    override def transitionRewardProb(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double) = {

//      var numOfCarsFirstLoc = scala.math.min(state.id._1 - action.value, 20)
//      var numOfCarsSecondLoc = scala.math.min(state.id._2 + action.value, 20)
//
//      val realRentalFirstLoc = scala.math.min(numOfCarsFirstLoc, rentalRequestFirstLoc)
//      val realRentalSecondLoc = scala.math.min(numOfCarsSecondLoc, rentalRequestSecondLoc)

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

      val prob = poisson(RENTAL_REQUEST_FIRST_LOC, req1) * poisson(RENTAL_REQUEST_SECOND_LOC, req2)*gridWorldPolicy.actionProb(state, action)
      val reward = (req1+req2)*RENTINCOME
      //println(prob, reward)
      (prob, (req1+req2)*RENTINCOME)

    }

    override def cost(state: gridWorldState, action: gridWorldAction, nextState: gridWorldState): Double = scala.math.abs(action.value * MOVINGCOST)

    override def applicableTransitions(state: gridWorldState): Seq[(gridWorldAction, gridWorldState)] = {
      val actions = gridWorldPolicy.applicableActions(state)
      val transactions = getCurrentStates.toArray
      for (action <- actions; nextState <- transactions) yield (action, nextState)
  }
    override def getCurrentStates:DenseMatrix[gridWorldState] = {
      if (!currentStates.isDefined) currentStates=Option(stateSpace)
      currentStates.get
    }
  }
   object gridWorldPolicy extends Policy[gridWorldState, gridWorldAction]{
     var policyCopy = DenseMatrix.tabulate[gridWorldAction] (X+1, Y+1){ (i, j) => new gridWorldAction { override val value: Int = 0} }
    var policy = DenseMatrix.tabulate[gridWorldAction] (X+1, Y+1){ (i, j) => new gridWorldAction { override val value: Int = 0} }
    override  def bestAction(state:gridWorldState):gridWorldAction = policy(state.id)

    override  def applicableActions(state: gridWorldState): Seq[gridWorldAction] = {
      val first2second = for (i <- 0 to scala.math.min(state.id._1, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = i
      }
      val second2first = for (i <- 1 to scala.math.min(state.id._2, MAXMOVE)) yield new gridWorldAction {
        override val value: Int = (-1) * i
      }
      first2second ++ second2first
    }

    override   def actionProb(state:gridWorldState, action:gridWorldAction):Double = {
      //1.0/applicableActions(state).size
      1.0
    }
     override def update(state:gridWorldState, action:gridWorldAction):Unit = {
       policyCopy(state.id) = policy(state.id) //make a copy
       policy(state.id)=action
     }
     override def isChanged:Boolean = {
       val same = policy.size == policyCopy.size  &&  {
         var i = 0
         while (i < policy.size && policy.toArray(i).value == policyCopy.toArray(i).value) i += 1
         i == policy.size
       }
       println(s"same=$same")
       !same
     }
  }

  import rl.core.mdp.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)
//  import rl.core.mdp.ValueFunctions.qlearning
//  qlearning.setDiscount(0.9).setLearningRate(0.5)

//  import rl.core.mdp.ValueFunctions.optimalValueIteration
//  optimalValueIteration.setDiscount(0.9)
  val result = gridWorldAgent.setEpoch(100)
    .setExitDelta(1.0)
    .setPolicyIteration(true)
    .observe(carRentalEnv, gridWorldPolicy)

  println(result.map(a => rounded(1, a.value)))
  println(gridWorldPolicy.policy.map(a => a.value))


}
