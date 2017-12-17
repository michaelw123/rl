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
object mdpTest extends App {
  val X = 5
  val Y = 5

  implicit object gridWorldEnv extends Environment[DenseMatrix, gridWorldState]{
    def stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
      (i,j) => new gridWorldState((i,j), 0.0)
    }
    def actionSpace:Seq[gridWorldAction]= Seq(North, East, South, West)
    def getStates:DenseMatrix[gridWorldState] = currentStates
    var currentStates = stateSpace
  }
  implicit val policy:gridWorldPolicy = new gridWorldPolicy {
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
        case (North, (_, 0)) => (state.id, -1)
        case (North, b) => ((b._1, b._2 - 1), 0)
        case (East, (XSIZE, _)) => (state.id, -1)
        case (East, b) => ((b._1 + 1, b._2), 0)
        case (South, (_, YSIZE)) => (state.id, -1)
        case (South, b) => ((b._1, b._2 + 1), 0)
        case (West, (0, _)) => (state.id, -1)
        case (West, b) => ((state.id._1 - 1, state.id._2), 0)
        case (_, _) => (state.id, 0)
      }
      (gridWorldEnv.getStates(r._1), r._2)
    }
  }
  //import rl.core.mdp.ValueFunctions.Bellman
  //Bellman.setDiscount(0.9)

   // import rl.core.mdp.ValueFunctions.optimalValueIteration
   // optimalValueIteration.setDiscount(0.9)

  import rl.core.mdp.ValueFunctions.qlearning
  qlearning.setDiscount(0.9)
      .setLearningRate(.5)
  gridWorldEnv.update(gridWorldEnv.stateSpace)
  val result = gridWorldAgent   //.setEpoch(0)
    .setExitValue(0.001)
    .observe


  println(result.map(a => rounded(3, a.value)))



}
