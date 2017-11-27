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
    def policy[T](a: T, b: (Int, Int)): ((Int, Int), Double) = (a, (b._1, b._2)) match {
      case (_, A) => (PRIMEA, 10)
      case (_, B) => (PRIMEB, 5)
      case (North, (0, _)) => ((0, b._2), -1)
      case (North, (_, _)) => ((b._1 - 1, b._2), 0)
      case (East, (_, WORLDSIZE1)) => ((b._1, WORLDSIZE1), -1)
      case (East, (_, _)) => ((b._1, b._2 + 1), 0)
      case (South, (WORLDSIZE1, _)) => ((WORLDSIZE1, b._2), -1)
      case (South, (_, _)) => ((b._1 + 1, b._2), 0)
      case (West, (_, 0)) => ((b._1, 0), -1)
      case (West, (_, _)) => ((b._1, b._2 - 1), 0)
    }
    case class Node(val x: Int, val y: Int) {

      var value: Double = 0

      var north = policy(North, (x, y))
      var east = policy(East, (x, y))
      val south = policy(South, (x, y))
      val west = policy(West, (x, y))
    }
    for (i <- 0 until 1000) {
      //bellman
      valueIteration
    }
    println(grid.map(a => (rounded(3, a.value))))





    def bellman = {
    val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    for (i <- 0 until WORLDSIZE; j <- 0 until WORLDSIZE) {
      newGrid(i,j).value += ACTIONPROB * (newGrid(i,j).north._2 + DISCOUNT * grid(newGrid(i,j).north._1._1, newGrid(i,j).north._1._2).value)
      newGrid(i,j).value += ACTIONPROB * (newGrid(i,j).east._2 + DISCOUNT * grid(newGrid(i,j).east._1._1, newGrid(i,j).east._1._2).value)
      newGrid(i,j).value += ACTIONPROB * (newGrid(i,j).south._2 + DISCOUNT * grid(newGrid(i,j).south._1._1, newGrid(i,j).south._1._2).value)
      newGrid(i,j).value += ACTIONPROB * (newGrid(i,j).west._2 + DISCOUNT * grid(newGrid(i,j).west._1._1, newGrid(i,j).west._1._2).value)
    }
    grid = newGrid
  }
  def rounded(x: Int, n:Double) = {
    val w = math.pow(10, x)
    (n * w).toLong.toDouble / w
  }
  def valueIteration = {
    val newGrid = DenseMatrix.tabulate[Node](WORLDSIZE, WORLDSIZE) {
      (i, j) => new Node(i, j)
    }
    for (i <- 0 until WORLDSIZE; j <- 0 until WORLDSIZE) {
      val v1 = (newGrid(i,j).north._2 + DISCOUNT * grid(newGrid(i,j).north._1._1, newGrid(i,j).north._1._2).value)
      val v2 =(newGrid(i,j).east._2 + DISCOUNT * grid(newGrid(i,j).east._1._1, newGrid(i,j).east._1._2).value)
      val v3 =(newGrid(i,j).south._2 + DISCOUNT * grid(newGrid(i,j).south._1._1, newGrid(i,j).south._1._2).value)
      val v4 = (newGrid(i,j).west._2 + DISCOUNT * grid(newGrid(i,j).west._1._1, newGrid(i,j).west._1._2).value)
      newGrid(i,j).value = max(Seq(v1, v2, v3, v4))
    }
    grid = newGrid


   // values.append(actionReward[i][j][action] + discount * world[newPosition[0], newPosition[1]])

  }

}
