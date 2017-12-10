package test.mdp

import rl.experiment.mdp.GridWorld
import rl.experiment.mdp.core._
import rl.experiment.mdp.core.ValueFunction.Bellman

/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */
object mdpTest1 extends App {

  implicit object gridWorldPolicy {

  }

  DenseMatrix[GrisWorldState] result = gridWorldAgent
   .setValueFunctions(Bellman)
    .setPolicy(gridWorldPolicy)
    .setEpoch(100)
    .fit

  println(result)



}
