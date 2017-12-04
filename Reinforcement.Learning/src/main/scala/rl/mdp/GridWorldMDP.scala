package rl.mdp
import breeze.linalg.DenseMatrix
import rl.core.mdp.MDP.{State, _}

import scala.collection.immutable.List

/**
  * Created by MichaelXiaoqun on 2017-12-03.
  */
object GridWorldMDP extends App{

  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action

  case class gridWorldReward(reward:Double) extends Reward[Double]
  case class gridWorldReward1(reward:Int) extends Reward[Int]
//  object gridWorldReward { self =>
//    def apply(r:Double) = new gridWorldReward(r)
//    def unapply(r:gridWorldReward):Double ={
//      println("asdjask")
//      r.reward
//    }
//  }

//  grisWorldEnvironment[S<:State, CS[S]] extends MDPEnvironment[S<:State, CS[S]]  {
//    def allActions: Seq[Action] = Seq(North, West, East, South)
//  }
//  class gridWorldValue[Double](v: Double) extends Valueable[Double]{
//    def unapply:Double = v
//
//  }
//
//  class gridWorldReward[Double](r: Double) extends Rewardable[Double] {
//    def unapply:Double = r
//  }
//
//
//  class gridWorldState[List[Action], gridWorldState, DenseMatrix[gridWorldState]]  extends Statable[List[Action], gridWorldState, IndexedSeq[gridWorldState]] {
//    val x:Int = 0
//    val y:Int = 0
//    def apply(a:Int, b:Int) = new gridWorldState(x, y)
//    def unapply = (x, y)
//    val xx = Seq(North, West, East, South)
//    def availableActions : Seq[A] = (x, y) match {
//      case (0, _) => Seq(East, South, West)
//      case (_, 0) => Seq(South, West, North)
//
//    }
//    def availableStates : IndexedSeq[gridWorldState] = IndexedSeq(new gridWorldState(0, 1))
//    def availableActions  = List(North, West)
//  }
def liftSomething2[T, R](f: (T, T) => R): (Reward[T], Reward[T]) => R = (a, b) => f(a.reward, b.reward)

  val sumInts = liftSomething2[Int, Int](_ + _)

  val xx = gridWorldReward(2)
  val yy = gridWorldReward(3)
  val aa = sumInts(gridWorldReward1(2), gridWorldReward1(3))

  val zz = xx match {
    case yy => "A"
    case _ => "B"
  }

  println(yy)
  println(xx)
  println(aa)
}