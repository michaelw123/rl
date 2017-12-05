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
import breeze.linalg._
import breeze.storage.Zero
import rl.core.mdp.MDP._

import scala.collection.immutable.List

/**
  * Created by Michael Wang on 2017-12-03.
  */
object GridWorldMDP extends App{

  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action

  class gridWorldReward(val reward:Double) extends Reward
  object gridWorldReward {
    //def lift[T, R](f: (T, T) => R): (Reward[T], Reward[T]) => R = (a, b) => f(a.reward, b.reward)
    //def apply(r:Double):gridWorldReward = gridWorldReward(r)
    def apply(r:Double):Double = r
    def unapply(reward:Double):Option[Double] = if (reward != 0.0) Some(reward) else None
    implicit def double2Reward(r:Double):gridWorldReward = new gridWorldReward(r)
  }

  class gridWorldValue(val value:Double) extends Value
  object gridWorldValue{
    def apply(v:Double):Double = v
    def unapply(v:Double):Option[Double] = if (v != 0) Some(v) else None
    implicit def double2Value(v:Double):gridWorldValue = new gridWorldValue(v)
  }

  class gridWorldState(val x:Int, val y:Int) extends State
  object gridWorldState extends Stateable[Action, gridWorldValue, gridWorldReward, gridWorldState] {
    override def availableActions:Seq[Action] = Seq(North, East, South, West)
    override def value:gridWorldValue = 0.0
    override def apply(action:Action):(gridWorldState, gridWorldReward) = (new gridWorldState(0,0), new gridWorldReward(0.0))

    def apply(xx:Int, yy:Int):(Int, Int) = (xx, yy)
    def unapply(state:gridWorldState):Option[(Int, Int)] = Some((state.x,  state.y))
  }

  object gridWorldPolicy extends StochasticPolicy[gridWorldState, Action] {
    def pi(state:gridWorldState, action:Action):Double = 0
  }
//  object gridWorldPolicy {
//
//  }

//  class gridWorldStateSpace(val width:Int=10, val height:Int=10) extends StateSpace {
//  object gridWorldStateSpace extends StateSpaceable [gridWorldState] {
//    def allStates:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](width, height) {
//      (i,j) =>new gridWorldState(i, j)
//    }
//  }



  //  grisWorldEnvironment[S<:State, CS[S]] extends MDPEnvironment[S<:State, CS[S]]  {
//    def allActions: Seq[Action] = Seq(North, West, East, South)
//  }

//  class gridWorldState[List[Action], gridWorldState, DenseMatrix[gridWorldState]]  extends Statable[List[Action], gridWorldState, IndexedSeq[gridWorldState]] {
//    val x:Int = 0
//    val y:Int = 0
//    def apply(a:Int, b:Int) = new gridWorldState(x, y)
//    def unapply = (x, y)
//    val xx = Seq(North, West, East, South)
//    def availableActions : Seq[A] = (x, y) match {
//      case (0, _) => Seq(East, South, West)
//      case (_, 0) => Seq(South, West, North)
//
//    }
//    def availableStates : IndexedSeq[gridWorldState] = IndexedSeq(new gridWorldState(0, 1))
//    def availableActions  = List(North, West)
//  }

  val state = new gridWorldState(0,1)
  val aa = gridWorldReward(9.0)

  val qq = aa match {
    case  gridWorldReward(9.1) => "match"
    case _ => "not match"
  }
//  println(yy)
//  println(xx)
  println(aa)
  println(qq)
  val p = gridWorldPolicy.pi(new gridWorldState(0,1), North)
  println(p)

}