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
import rl.core.mdp.MDP._

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

  class gridWorldState(val x:Int, val y:Int) extends State with  Stateable[Action, gridWorldValue, gridWorldReward, gridWorldState] {
    override def availableActions:Seq[Action] = gridWorldAgent.allActions
    override def value:gridWorldValue = 0.0
    override def apply(action:Action):(gridWorldState, gridWorldReward) = (new gridWorldState(0,0), new gridWorldReward(0.0))
  }
  object gridWorldState{
    def apply(xx:Int, yy:Int):(Int, Int) = (xx, yy)
    def unapply(state:gridWorldState):Option[(Int, Int)] = Some((state.x,  state.y))
  }

  object gridWorldPolicy extends StochasticPolicy[gridWorldState, Action] {
    def pi(state:gridWorldState, action:Action):Double = 0
  }
  implicit object hellmanAlgorithm extends Algorithm[BellmanConfig] {
    def run(config:BellmanConfig) = {
      val states = config.allStates

      states.toArray.foreach { state =>
        state.value = state.availableActions.foldLeft(state.value)((a,b) =>  a + config.getActionProb * (b + config.getDiscount * states(a.x).value))
      }

    }
  }
   class BellmanConfig extends MDPConfiguration {
    private var X=0
    private var Y=0
     private var actionProb=0.0

     private var discount=0.0
    def allStates:DenseMatrix[gridWorldState]=DenseMatrix.tabulate[gridWorldState](X,Y){
      (i,j) => new gridWorldState(i,j)
    }
    val allActions = Seq(North, East, South, West)
    def setX(value:Int): this.type ={
      X=value
      this
    }
    def setY(value:Int): this.type ={
      Y=value
      this
    }
     def setActionProb(value:Double) {
       actionProb = value
       this
     }
     def setDiscount(value:Double) {
       discount = value
       this
     }
     def getDiscount = discount
    def getActionProb =  actionProb
    def getX= X
    def getY= Y
  }
  object gridWorldAgent extends Agent [gridWorldState, Action, gridWorldReward, gridWorldValue] {
    var config:BellmanConfig= (None : Option[BellmanConfig]).orNull
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PRIMEB = (2, 3)
    def setConfig(conf:BellmanConfig) = {
      config = conf
      this

    }
    def decision(state:gridWorldState, action:Action):(gridWorldState, gridWorldReward) = {
      val x = config.getX
      val y = config.getY
      val r = (action, (state.x, state.y)) match  {
        case (_, A) => (PRIMEA, 10)
        case (_, B) => (PRIMEB, 5)
        case (North, (0, _)) => ((state.x, state.y), -1)
        case (North, b) => ((b._1 - 1, b._2), 0)
        case (East, (_, y)) => ((state.x, state.y), -1)
        case (East, b) => ((b._1, b._2 + 1), 0)
        case (South, (x, _)) => ((state.x, state.y), -1)
        case (South, b) => ((b._1 + 1, b._2), 0)
        case (West, (_, 0)) => ((state.x, state.y), -1)
        case (West, b) => ((b._1, b._2 - 1), 0)
        case (_, _) => ((state.x, state.y), 0)
      }
      (new gridWorldState(r._1._1, r._1._2), new gridWorldReward(r._2))
    }

    def runAlgorithm[T](config:T) (implicit algo:Algorithm[T]) ={
      algo.run(config)
    }
    def allActions = config.allActions
  }

}