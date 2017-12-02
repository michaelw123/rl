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

package rl.mdp

import breeze.linalg.{DenseMatrix, DenseVector, max}

/**
  * Created by Michael Wang on 2017-11-26.
  */
object gridWorld extends App {
  trait Action
  sealed
  case class North(value:Int=0) extends Action
  case class East(value:Int=1) extends Action
  case class South(value:Int=2) extends Action
  case class West(value:Int=3) extends Action

  val DISCOUNT = 0.9
  val ACTIONPROB=0.25
    val WORLDSIZE = 5
    val WORLDSIZE1 = WORLDSIZE-1
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PRIMEB = (2, 3)
    var grid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    def policy[T](a: T, b: (Int, Int)): ((Int, Int), Double) = (a, b) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (North, (0, _)) => (b, -1)
      case (North, b) => ((b._1 - 1, b._2), 0)
      case (East, (_, WORLDSIZE1)) => (b, -1)
      case (East, b) => ((b._1, b._2 + 1), 0)
      case (South, (WORLDSIZE1, _)) => (b, -1)
      case (South, b) => ((b._1 + 1, b._2), 0)
      case (West, (_, 0)) => (b, -1)
      case (West, b) => ((b._1, b._2 - 1), 0)
      case (_, _) => (b, 0)
    }

    case class Node(val x: Int, val y: Int) {
      var value: Double = 0
      val actions = Seq( policy(North, (x, y)),
                policy(East, (x, y)),
                policy(South, (x, y)),
                policy(West, (x, y)))
    }
    for (i <- 0 until 5000) {
      bellman
     // valueIteration
    }
    println(grid.map(a => rounded(3, a.value)))

  def bellman = {
      val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
        (i, j) =>  Node(i, j)
      }
      newGrid.toArray.foreach { node =>
        node.value = node.actions.foldLeft(node.value)((a,b) =>  a + ACTIONPROB * (b._2 + DISCOUNT * grid(b._1).value))
      }
      grid = newGrid
  }
  def valueIteration = {
    val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    newGrid.toArray.foreach { node =>
      node.value = max(node.actions.map(a => a._2 + DISCOUNT * grid(a._1).value))
    }
    grid = newGrid
  }
  def rounded(x: Int, n:Double) = {
    val w = math.pow(10, x)
    (n * w).toLong.toDouble / w
  }
}
