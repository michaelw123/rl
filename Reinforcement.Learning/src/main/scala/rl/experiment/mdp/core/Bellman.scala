package rl.experiment.mdp.core

import breeze.linalg.DenseMatrix
//import rl.mdp.GridWorldMDP.{BellmanConfig, gridWorldState}

//import rl.experiment.rl.core.Configuration
/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */

object ValueFunction {
  implicit object Bellman extends ValueFunction[State, DenseMatrix[_]] {

    val allStates:DenseMatrix[State] = DenseMatrix.zeros[State](X, Y)
    val allActions:Seq[Action]=Seq(???)


    private var discount=0.9

    def setDiscount(value:Double): this.type ={
      discount = value
      this
    }

    def getDiscount = discount


    override def observe(state:State):DenseMatrix[State] = ???
  }

}
