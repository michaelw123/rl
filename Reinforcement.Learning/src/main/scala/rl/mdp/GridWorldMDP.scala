package rl.mdp

import rl.mdp.MDP.Reward

/**
  * Created by MichaelXiaoqun on 2017-12-03.
  */
class GridWorldMDP {

  import MDP.{Action, Value}

  sealed
  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action

  class gridWorldValue(value: Double) extends Value

  class gridWorldReward(reward: Double) extends Reward

  case class State(i:Int, j:Int)

}