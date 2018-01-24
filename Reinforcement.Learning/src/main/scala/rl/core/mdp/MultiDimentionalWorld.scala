package rl.core.mdp

import breeze.linalg.DenseMatrix
import rl.utils.rounded

/**
  * Created by wangmich on 01/24/2018.
  */

object MultiDimentionalWorld {
  type StateID = (Int, Int, Int)
  class multiDimentionalState(val id:StateID, var value:Double) extends State[StateID]
  object multiDimentionalAgent extends Agent[Action, Array, multiDimentionalState] {
    def observe[VF <: ValueFunction, P <: Policy[multiDimentionalState, Action], E <: Environment[Array, multiDimentionalState, Action]](env: E, policy: P)(implicit vf: VF): Array[multiDimentionalState] = {
      def observeOnce: Array[multiDimentionalState] = {
        val newStates = env.stateSpace
        newStates.map(state => {
          val action = policy.optimalPolicy(state)
          val vrp = env.stochasticRewards(state, action).map(x => (x._1, x._2, x._3 * policy.actionProb(state, action)))
          state.value = vf.value(state, vrp) - env.cost(state, action)
        })
        newStates
      }

      for (i <- 0 until epoch) {
        val newStates = observeOnce
        env.update(newStates)
        val r = newStates.map(a => rounded(1, a.value))
        println(s"Epoch $i: $r")
      }
      env.getCurrentStates
    }
  }
}
