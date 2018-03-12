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
import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldState}
import rl.core.mdp.{State, _}
import rl.core.mdp.MultiDimentionalWorld._

/**
  * Created by wangmich on 01/24/2018.
  */
object blackJack extends App {
  val X = 11 *2
  val Y = 11 *2
  class blackJackState(val id:(Int, Int), var value:Double) extends State[(Int, Int)] {
    def isHandSoft = id._1 < 11
    def isDealerSoft = id._1 < 11
    def dealerSum = id._2
    def sum = id._1
  }
  implicit def grid2BlackJackState(s:gridWorldState):blackJackState = new blackJackState(s.id, s.value)

//  object blackJackEnv extends Environment[DenseMatrix, blackJackState, gridWorldAction] {
//    //X: sum of cards, 2 <= sum <= 21. keep drawing until sum is at least 11, so 11 <= sum <= 21.
//    //Thus, i=0 is burst; i shows sum i  or i+10 (soft when i<=10)
//    // Y: sum of dealer's showing card. j=0: burst, j shows sum j  and j+10 (soft when j<=10)
//    def stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X+1, Y+1) { (i, j) => new gridWorldState((i, j), 0) }
//  }
}
