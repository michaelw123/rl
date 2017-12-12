package rl.experiment.mdp

import breeze.linalg.DenseMatrix
import rl.experiment.mdp.core.{Environment, _}
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
    override def reward(state:gridWorldState, action:gridWorldAction):(gridWorldState, Double) = ???
    override def availableActions(state: gridWorldState): Seq[gridWorldAction] = Seq(North, East, South, West)
    override def getActionProb(action:gridWorldAction):Double = 0.25
  }

  class gridWorldState(val id:(Int, Int), var value:Double) extends State[(Int, Int)]

  object gridWorldAgent extends Agent[gridWorldAction, gridWorldState] {
    //private var env:Environment[gridWorldAction, gridWorldState, DenseMatrix[gridWorldState]] = ???
    //     def setEnvironment(value: Environment[gridWorldAction, gridWorldState, DenseMatrix[gridWorldState]]):this.type = {
    //      env = value
    //      this
    //    }
        override def observe[VF <: ValueFunction, P <: Policy[gridWorldState, gridWorldAction], E <: Environment[gridWorldAction, gridWorldState]] (implicit vf:VF, policy:P, env:E): DenseMatrix[gridWorldState] = {
          var resultState:DenseMatrix[gridWorldState] =env.stateSpace
          for (i <- 0 until epoch) {
            val allStates = env.stateSpace
            allStates.map(state => {
              state.value = policy.availableActions(state)
                .foldLeft(state.value)((a, b) => {
                  val (nextState, reward) = policy.reward(state, b)
                  val actionProb = policy.getActionProb(b)
                  vf.value(state.value, resultState(nextState.id).value, reward, actionProb)
                })
            })
            resultState = allStates
          }
      resultState
    }
    private var epoch = 10
    def setEpoch(value:Int) : this.type ={
      epoch = value
      this
    }
  }


}
