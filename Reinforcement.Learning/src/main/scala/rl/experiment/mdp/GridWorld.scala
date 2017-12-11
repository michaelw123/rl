package rl.experiment.mdp

import breeze.linalg.DenseMatrix
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

  class gridWorldState(val x:Int, val y:Int, val value:Double) extends State {
    type I = (Int, Int)
    val id:I = (x, y)
  }




  object gridWorldAgent extends Agent[gridWorldAction, gridWorldState, gridWorldPolicy, DenseMatrix[_], Environment[A, S, P, CS[S]], VF]{

    def setPolicy(value:gridWorldPolicy) = ???

    override def observe[VF](state: gridWorldState)(implicit vf: VF): DenseMatrix[gridWorldState] = {

    }

  }


}
