package test.mdp

import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldState}
import rl.utils.poisson

/**
  * Created by wangmich on 12/21/2017.
  */
object test1 extends App {
//  var prob = 0.0
//  for (i <- 0 until scala.math.min(10, 6)) {
//    val p = poisson(3, i) * poisson(3, scala.math.abs(0 -i))
//    prob += p
//    println(i, p, prob)
//  }
//  println(prob)
//
//  println(poisson(3,5))
//  (state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double)

  val  state:gridWorldState=new gridWorldState((18, 17), 0)

  val actionState = carRentalClient.carRentalEnv.availableTransitions(state)
  var vrp = Seq[(Double, Double, Double)]()
  for ((action, nextState) <- actionState) {
    val (actionProb, reward ) = carRentalClient.carRentalEnv.transitionRewardProb(state, action, nextState)
    if (reward!=0 && actionProb!=0) {
      vrp = vrp :+ (nextState.value, reward - carRentalClient.carRentalEnv.cost(state, action, nextState), actionProb)
    }
  }
  println("aaa")
//  /state.value = vf.value(state, vrp)
}
