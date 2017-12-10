package rl.experiment.mdp

import breeze.linalg.DenseMatrix
import rl.experiment.mdp.core._
/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */
object GridWorld {
  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action

  object gridWorldPolicy extends Policy[gridWorldState, Action] {
    override def reward(state:gridWorldState, action:Action):(State, Double) = ???
    override def availableActions(state: gridWorldState): Seq[Action] = Seq(North, East, South, West)
  }

  class gridWorldState(val x:Int, val y:Int, val value:Double) extends State {
    type I = (Int, Int)
    val id:I = (x, y)
  }




  object gridWorldAgent extends Agent[State, Action, Policy, DenseMatrix]{
    def setPolicy(value:Policy) = ???

  }


}
