package rl.mdp

import java.awt.Desktop.Action

import breeze.linalg.{DenseMatrix, DenseVector, max}

/**
  * Created by MichaelXiaoqun on 2017-11-26.
  */
object gridWorld extends App {
  trait Action
  sealed
  case class North(value:Int=0) extends Action
  case class East(value:Int=1) extends Action
  case class South(value:Int=2) extends Action
  case class West(value:Int=3) extends Action

  val DISCOUNT = 0.9
  val ACTIONPROB=0.25
    val WORLDSIZE = 5
    val WORLDSIZE1 = WORLDSIZE-1
    val A = (0, 1)
    val PRIMEA = (4, 1)
    val B = (0, 3)
    val PRIMEB = (2, 3)
    var grid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    def policy[T](a: T, b: (Int, Int)): ((Int, Int), Double) = (a, b) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (North, (0, _)) => (b, -1)
      case (North, b) => ((b._1 - 1, b._2), 0)
      case (East, (_, WORLDSIZE1)) => (b, -1)
      case (East, b) => ((b._1, b._2 + 1), 0)
      case (South, (WORLDSIZE1, _)) => (b, -1)
      case (South, b) => ((b._1 + 1, b._2), 0)
      case (West, (_, 0)) => (b, -1)
      case (West, b) => ((b._1, b._2 - 1), 0)
      case (_, _) => (b, 0)
    }

    case class Node(val x: Int, val y: Int) {
      var value: Double = 0

      val north = policy(North, (x, y)) //next position (x,y), and reward
      val east = policy(East, (x, y))
      val south = policy(South, (x, y))
      val west = policy(West, (x, y))
    }
    for (i <- 0 until 5000) {
     // bellman
      valueIteration
    }
    println(grid.map(a => rounded(3, a.value)))

  def bellman = {
      val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
        (i, j) =>  Node(i, j)
      }
      newGrid.toArray.foreach { node =>
        node.value += ACTIONPROB * (node.north._2 + DISCOUNT * grid(node.north._1).value)
        node.value += ACTIONPROB * (node.east._2 + DISCOUNT * grid(node.east._1).value)
        node.value += ACTIONPROB * (node.south._2 + DISCOUNT * grid(node.south._1).value)
        node.value += ACTIONPROB * (node.west._2 + DISCOUNT * grid(node.west._1).value)
      }
      grid = newGrid
  }
  def valueIteration = {
    val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    newGrid.toArray.foreach { node =>
      val v1 = node.north._2 + DISCOUNT * grid(node.north._1).value
      val v2 =node.east._2 + DISCOUNT * grid(node.east._1).value
      val v3 =node.south._2 + DISCOUNT * grid(node.south._1).value
      val v4 = node.west._2 + DISCOUNT * grid(node.west._1).value
      node.value = max(Seq(v1, v2, v3, v4))
    }
    grid = newGrid
  }
  def rounded(x: Int, n:Double) = {
    val w = math.pow(10, x)
    (n * w).toLong.toDouble / w
  }
}
