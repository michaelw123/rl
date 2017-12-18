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
import rl.core.mdp.GridWorld.{gridWorldAgent, gridWorldPolicy, gridWorldState, gridWorldAction}
import rl.utils.rounded
/**
  * Created by Michael Wang on 12/18/2017.
  *
  * This program is an application to solve Car Rental problem in Sunnton/Barto's book
  * "Reinforcement Learning: An Introduction (2nd Edition)" chapter 4, on the MDP framework developed in this project using gridWorld model
  */
object carRentalClient extends App {
  val X = 20
  val Y = 20
  implicit object carRentalEnv extends Environment[DenseMatrix, gridWorldState, gridWorldAction]{
    val stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X, Y) { (i,j) => new gridWorldState((i,j), 0)}
    val actionSpace:Seq[gridWorldAction]= Seq(new gridWorldAction{override val value:Int= -5}, new gridWorldAction{override val value:Int= -4},
                      new gridWorldAction{override val value:Int= -3}, new gridWorldAction{override val value:Int= -2},
                      new gridWorldAction{override val value:Int= -2}, new gridWorldAction{override val value:Int= 0},
                      new gridWorldAction{override val value:Int= 1}, new gridWorldAction{override val value:Int= 2},
                      new gridWorldAction{override val value:Int= 3}, new gridWorldAction{override val value:Int= 4},
                      new gridWorldAction{override val value:Int= 5})
    def getStates:DenseMatrix[gridWorldState] = currentStates
    var currentStates = stateSpace
    override def reward(state: gridWorldState, action: gridWorldAction): (gridWorldState, Double) = {
      (new gridWorldState((0,0), 0), 0)
    }

  }
}