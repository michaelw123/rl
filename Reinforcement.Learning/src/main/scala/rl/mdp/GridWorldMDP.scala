package rl.mdp
import breeze.linalg.DenseMatrix
import rl.core.mdp.MDP._


import scala.collection.immutable.List

/**
  * Created by MichaelXiaoqun on 2017-12-03.
  */
object GridWorldMDP extends App{

  case object North extends Action
  case object East extends Action
  case object South extends Action
  case object West extends Action

  class gridWorldReward extends Reward
  object gridWorldReward {
    //def lift[T, R](f: (T, T) => R): (Reward[T], Reward[T]) => R = (a, b) => f(a.reward, b.reward)
    def apply(r:Double):gridWorldReward = gridWorldReward(r)
    def unapply(reward:Double):Option[Double] = if (reward != 0.0) Some(reward) else None
    implicit def double2Reward(r:Double):gridWorldReward = apply(r)
  }

  class gridWorldValue extends Value
  object gridWorldValue{
    def apply(v:Double):gridWorldValue = gridWorldValue(v)
    def unapply(v:Double):Option[Double] = if (v != 0) Some(v) else None
    implicit def double2Value(v:Double):gridWorldValue = apply(v)
  }

  class gridWorldState extends State[Action, gridWorldValue, gridWorldReward, gridWorldState] {
    override def availableActions:Seq[Action] = Seq(North, East, South, West)
    override def value:gridWorldValue = 0.0
    override def transition(action:Action):(gridWorldState, Reward) = (gridWorldState, gridWorldReward(0))
  }
  object gridWorldState {

  }


  //  grisWorldEnvironment[S<:State, CS[S]] extends MDPEnvironment[S<:State, CS[S]]  {
//    def allActions: Seq[Action] = Seq(North, West, East, South)
//  }

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

//  val xx= gridWorldReward(2.0)
//  val yy = gridWorldReward(3.0)
//  val mm:Double = 3.0
// // val aa = xx + yy
//
//  val zz = xx match {
//    case `yy` => "A"
//    case _ => "B"
//  }
  val aa = gridWorldReward(9.0)
  val qq = aa match {
    case  gridWorldReward(8.0)=> "A"
    case _ => "B"
  }
//  println(yy)
//  println(xx)
  println(aa)
  println(qq)


}