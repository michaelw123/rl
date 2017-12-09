package test.mdp

import rl.experiment.mdp.GridWorld
import rl.experiment.mdp.core._
import rl.experiment.mdp.core.ValueFunction.BellmanConfig

/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */
object mdpTest1 extends App {

  BellmanConfig.setDiscount(10)
  implicit object gridWorldPolicy {

  }

  DenseMatrix[GrisWorldState] result = gridWorldAgent
   .setValueFunctions(Bellman)
    .setEpoch(100)
    .run

  println(result)



}
