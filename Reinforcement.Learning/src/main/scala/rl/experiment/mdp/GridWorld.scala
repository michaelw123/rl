package rl.experiment.mdp

import breeze.linalg.DenseMatrix
import rl.experiment.mdp.GridWorld.gridWorldAction.North
import rl.experiment.mdp.GridWorld.gridWorldPolicy
import rl.experiment.mdp.core._
/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */
object GridWorld {
  trait gridWorldAction extends Action
  object gridWorldAction {
    case object North extends gridWorldAction
    case object East extends gridWorldAction
    case object South extends gridWorldAction
    case object West extends gridWorldAction
  }


  class gridWorldPolicy extends Policy[gridWorldState, gridWorldAction] {
    import gridWorldAction._
    override def reward(state:gridWorldState, action:gridWorldAction):(State, Double) = ???
    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = Seq(North, East, South, West)
  }

  class gridWorldState(val x:Int, val y:Int, var value:Double) extends State {
    type I = (Int, Int)
    val id:I = (x, y)
  }




  class gridWorldAgent extends Agent[gridWorldAction, gridWorldState, gridWorldPolicy, DenseMatrix[_], Environment[gridWorldAction, gridWorldState, gridWorldPolicy, DenseMatrix[_]], ValueFunction]{
    private var policy:gridWorldPolicy = ???
    def setPolicy(value:gridWorldPolicy):this.type = {
      policy=value
      this
    }
    private var env:Environment[gridWorldAction, gridWorldState, gridWorldPolicy, DenseMatrix[_]] = ???
    override def observe[VF](state: gridWorldState)(implicit vf: ValueFunction): DenseMatrix[gridWorldState] = {
      val allStates =env.allStates
      val actions = policy.availableActions(state)
      val (nextState, reward) = policy.reward(state, North)
      val value = vf.value(state.value, nextState.value, reward, 0.25)
      state.value = value
      allStates

    }

  }


}
