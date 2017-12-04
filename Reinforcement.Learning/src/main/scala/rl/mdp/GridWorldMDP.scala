package rl.mdp
import breeze.linalg.DenseMatrix
import rl.core.mdp.MDP._

import scala.collection.immutable.List

/**
  * Created by MichaelXiaoqun on 2017-12-03.
  */
object GridWorldMDP extends App{


   object North extends Action
   object East extends Action
   object South extends Action
   object West extends Action

  class gridWorldValue[Double](v: Double) extends Valueable[Double]{
    def unapply:Double = v

  }

  class gridWorldReward[Double](r: Double) extends Rewardable[Double] {
    def unapply:Double = r
  }


  class gridWorldState[List[Action], gridWorldState, DenseMatrix[gridWorldState]]  extends Statable[List[Action], gridWorldState, IndexedSeq[gridWorldState]] {
    val x:Int = 0
    val y:Int = 0
//    def apply(a:Int, b:Int) = new gridWorldState(x, y)
//    def unapply = (x, y)
    val xx = Seq(North, West, East, South)
//    def availableActions : Seq[A] = (x, y) match {
//      case (0, _) => Seq(East, South, West)
//      case (_, 0) => Seq(South, West, North)
//
//    }
//    def availableStates : IndexedSeq[gridWorldState] = IndexedSeq(new gridWorldState(0, 1))
    //def availableActions  = List(North, West)
  }
  val xx = Seq(North, West, East, South)

  println(xx)
}