package rl.mdp

import java.awt.Desktop.Action

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * Created by MichaelXiaoqun on 2017-11-26.
  */
object gridWorld {
  trait Action
  sealed
  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action



  class GridWorld {
    val WORLDSIZE = 5
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PROMEB = (2, 3)
   val grid = DenseMatrix.fill[State](WORLDSIZE, WORLDSIZE)(State(North,0,0,0.1))
  }
  val actions = DenseVector(North, East, South, West)
  case class State (action:Action, x:Int, y:Int, reward:Double)
  val s=State(North, 0, 0, 0.2)







}
