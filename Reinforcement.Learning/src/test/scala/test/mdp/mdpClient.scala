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

import rl.core.mdp.GridWorld.gridWorldAction._
import rl.core.mdp.GridWorld._
import rl.core.mdp._
import breeze.linalg.DenseMatrix
import rl.utils.rounded

/**
  * Created by Michael Wang on 2017-12-09.
  */
object mdpClient extends App {
  val X = 5
  val Y = 5

   object gridWorldEnv extends Environment[DenseMatrix, gridWorldState, gridWorldAction]{
    def stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
      (i,j) => new gridWorldState((i,j), 0.0)
    }
    def actionSpace:Seq[gridWorldAction]= Seq(new North, new East, new South, new West)
    def getStates:DenseMatrix[gridWorldState] = getCurrentStates
    //var currentStates = stateSpace
    override def reward(state: gridWorldState, action: gridWorldAction): (gridWorldState, Double) = {
      val A = (0, 1)
      val PRIMEA = (4, 1)
      val B = (0, 3)
      val PRIMEB = (2, 3)
      val XSIZE = X - 1
      val YSIZE = Y - 1
      val r = (action, state.id) match {
        case (_, A) => (PRIMEA, 10)
        case (_, B) => (PRIMEB, 5)
        case (a: North, (_, 0)) => (state.id, -1)
        case (a: North, b) => ((b._1, b._2 - 1), 0)
        case (a: East, (XSIZE, _)) => (state.id, -1)
        case (a: East, b) => ((b._1 + 1, b._2), 0)
        case (a: South, (_, YSIZE)) => (state.id, -1)
        case (a: South, b) => ((b._1, b._2 + 1), 0)
        case (a: West, (0, _)) => (state.id, -1)
        case (a: West, b) => ((state.id._1 - 1, state.id._2), 0)
        case (_, _) => (state.id, 0)
      }
      (gridWorldEnv.getStates(r._1), r._2)
    }
    override def transitionProb(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):Double  = 0.25
    override def cost(state:gridWorldState, action:gridWorldAction):Double = 0.0
    override def reward(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):Double  = reward(state, action)._2

    override def rewards(state:gridWorldState, action:gridWorldAction):Seq[(Double, Double, Double)] = {
      val stateReward = reward(state, action)
      val vrp = (stateReward._1.value, stateReward._2, 1.0)
      Seq[(Double, Double, Double)](vrp)
    }
    override def cost(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):Double  = cost(state, action)
    override def availableTransitions(state:gridWorldState):Seq[(gridWorldAction, gridWorldState)] = {
      val actions = gridWorldPolicy.availableActions(state)
      for (action <- actions) yield (action, reward(state, action)._1)
    }
    //override def gridWorldPolicyavailableActions(state:gridWorldState):Seq[gridWorldAction] = Seq(new North, new East, new South, new West)
    override def transitionRewardProb(state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double) = {
      (0.25, reward(state, action, nextState))
    }
  }

   object gridWorldPolicy extends Policy[gridWorldState, gridWorldAction]{
    val policy = DenseMatrix.tabulate[gridWorldAction] (X+1, Y+1){ (i, j) => new gridWorldAction { override val value: Int = 0} }
    def bestAction(state:gridWorldState):gridWorldAction = policy(state.id)

    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = {
      Seq(new North, new East, new West, new South)
    }

    override def actionProb(state:gridWorldState, action:gridWorldAction):Double = {
      1.0/availableActions(state).size
    }
  }

  import rl.core.mdp.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)

//    import rl.core.mdp.ValueFunctions.optimalValueIteration
//    optimalValueIteration.setDiscount(0.9)

// import rl.core.mdp.ValueFunctions.qlearning
//  qlearning.setDiscount(0.9)
//      .setLearningRate(.5)
  gridWorldEnv.update(gridWorldEnv.stateSpace)
  val result = gridWorldAgent.setEpoch(1000)
    //.setExitDelta(0.001)
    .observe(gridWorldEnv, gridWorldPolicy)


  println(result.map(a => rounded(3, a.value)))


}
