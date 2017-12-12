package rl.experiment.mdp.core

import breeze.linalg.DenseMatrix
//import rl.mdp.GridWorldMDP.{BellmanConfig, gridWorldState}

//import rl.experiment.rl.core.Configuration
/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */

object ValueFunctions {

  implicit object Bellman extends ValueFunction {
    private var discount = 0.0

    override def setDiscount(value: Double): this.type = {
      discount = value
      this
    }

    override def getDiscount = discount

    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      (statevalue + prob * (reward + discount * nextStateValue))
    }
  }

  implicit object optimalValueIteration extends ValueFunction {
    private var discount = 0.0

    override def setDiscount(value: Double): this.type = {
      discount = value
      this
    }

    override def getDiscount = discount

    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      reward + discount * nextStateValue
    }
  }

}


