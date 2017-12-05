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

  class gridWorldReward(val reward:Double) extends Reward
  object gridWorldReward {
    //def lift[T, R](f: (T, T) => R): (Reward[T], Reward[T]) => R = (a, b) => f(a.reward, b.reward)
    //def apply(r:Double):gridWorldReward = gridWorldReward(r)
    def apply(r:Double):Double = r
    def unapply(reward:Double):Option[Double] = if (reward != 0.0) Some(reward) else None
    implicit def double2Reward(r:Double):gridWorldReward = new gridWorldReward(r)
  }

  class gridWorldValue(val value:Double) extends Value
  object gridWorldValue{
    def apply(v:Double):Double = v
    def unapply(v:Double):Option[Double] = if (v != 0) Some(v) else None
    implicit def double2Value(v:Double):gridWorldValue = new gridWorldValue(v)
  }

  class gridWorldState(val x:Int, val y:Int) extends State[Action, gridWorldValue, gridWorldReward, gridWorldState] {
    override def availableActions:Seq[Action] = Seq(North, East, South, West)
    override def value:gridWorldValue = 0.0
    override def apply(action:Action):(gridWorldState, gridWorldReward) = (new gridWorldState(0,0), new gridWorldReward(0.0))
  }
  object gridWorldState {
    def apply(xx:Int, yy:Int):(Int, Int) = (xx, yy)
    def unapply(state:gridWorldState):Option[(Int, Int)] = Some((state.x,  state.y))
    //implicit def tuple2State(x:Int, y:Int):gridWorldState = new gridWorldState(x, y)
  }

  class gridWorldPolicy[gridWorldState, Action] extends StochasticPolicy[gridWorldState, Action] {
    def pi(state:gridWorldState, action:Action):Double = 0
  }
  object gridWorldPolicy {

  }

  class gridWorldStateSpace[gridWorldState] extends StateSpace[gridWorldState] {
    def allStates:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](10, 10) {
      (i, j) => new gridWorldState(i, j)
    }
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

  val state = gridWorldState(0,1)
  val aa = gridWorldReward(9.0)

  val qq = aa match {
    case  gridWorldReward(9.1) => "match"
    case _ => "not match"
  }
//  println(yy)
//  println(xx)
  println(aa)
  println(qq)


}