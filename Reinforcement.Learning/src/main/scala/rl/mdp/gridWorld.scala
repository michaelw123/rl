package rl.mdp

import java.awt.Desktop.Action

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * Created by MichaelXiaoqun on 2017-11-26.
  */
object gridWorld extends App {
  trait Action

  case class North(value:Int=0) extends Action
  case class East(value:Int=1) extends Action
  case class South(value:Int=2) extends Action
  case class West(value:Int=3) extends Action

  trait GridWorld {
    val WORLDSIZE = 4
//    val grid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE)
//      {
//      (i, j) => new Node(i, j)
//    }

    def policy [T <: Action](a:T, b:(Int, Int)):((Int, Int), Double)

/*
    def policy(a: Action, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (North, (0, _)) => ((0, b._2), 0)
      case (North, (_, _)) => ((b._1 - 1, b._2), 0)
      case (East, (_, WORLDSIZE)) => ((b._1, WORLDSIZE), 0)
      case (East, (_, _)) => ((b._1, b._2 + 1), 0)
      case (South, (WORLDSIZE, _)) => ((WORLDSIZE, b._2), 0)
      case (South, (_, _)) => ((b._1 + 1, b._2), 0)
      case (West, (_, 0)) => ((b._1, 0), 0)
      case (West, (_, _)) => ((b._1, b._2 - 1), 0)
    }
  */
  }
  object GridWorld {
    val WORLDSIZE = 4
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PRIMEB = (2, 3)

    implicit def northPolicy[North](a: North, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (North, (0, _)) => ((0, b._2), 0)
      case (North, (_, _)) => ((b._1 - 1, b._2), 0)
    }
    implicit def eastPolicy[East](a: East, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (East, (_, WORLDSIZE)) => ((b._1, WORLDSIZE), 0)
      case (East, (_, _)) => ((b._1, b._2 + 1), 0)
    }
    implicit def southPolicy[South](a: South, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (South, (WORLDSIZE, _)) => ((WORLDSIZE, b._2), 0)
      case (South, (_, _)) => ((b._1 + 1, b._2), 0)
    }
    implicit def westPolicy[West](a: West, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (West, (_, 0)) => ((b._1, 0), 0)
      case (West, (_, _)) => ((b._1, b._2 - 1), 0)
    }
  }
  class Node[T <: Action] (x: Int, y: Int)(implicit  f: (T, (Int, Int)) => ((Int, Int), Double)) {
      var value: Double = 0
      var north = f(North, (x, y))
      var east = f(East, (x, y))
      val south = f(South, (x, y))
      val west = f(West, (x, y))
  }

val node = new Node(2,3)

}
