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
  val WORLDSIZE = 4

  class GridWorld {
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PROMEB = (2, 3)
    val grid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE){
      (i,j) => new Node(i,j)
    }
  }
  class Node(x:Int, y:Int) {
    var value:Double=0
    var north = (x, y) match {
      case (0, _) => ((0, y), 0)
      case (_, _) => ((x - 1, y), 0)
    }
    var east = (x, y) match {
      case (_, WORLDSIZE) => ((x, WORLDSIZE), 0)
      case (_, _) => ((x, y+1), 0)
    }
    val south= (x, y) match {
      case (WORLDSIZE, _) => ((WORLDSIZE, y), 0)
      case (_, _) => ((x+1, y), 0)
    }
    val west = (x, y) match {
      case (_, 0) => ((x, 0), 0)
      case (_, _) => ((x, y-1), 0)
    }
  }









}
