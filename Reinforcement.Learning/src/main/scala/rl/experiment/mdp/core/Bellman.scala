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


    private var X=0
    private var Y=0
    private var actionProb=0.1
    private var discount=0.9
    private var episodes = 0
    def setX(value:Int): this.type ={
      X=value
      this
    }
    def setY(value:Int): this.type ={
      Y=value
      this
    }
    def setActionProb(value:Double): this.type ={
      actionProb = value
      this
    }
    def setDiscount(value:Double): this.type ={
      discount = value
      this
    }
    def setEpisodes(value:Int ): this.type = {
      episodes = value
      this
    }
    def getEpisodes = episodes
    def getDiscount = discount
    def getActionProb =  actionProb
    def getX= X
    def getY= Y

    override def value(state:State):DenseMatrix[State] = ???
  }

}
