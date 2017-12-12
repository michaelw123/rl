package test.mdp

import rl.experiment.mdp.GridWorld.gridWorldAction._
import rl.experiment.mdp.GridWorld._
import rl.experiment.mdp.core._
import breeze.linalg.DenseMatrix
import rl.utils.rounded

/**
  * Created by Michael Wang on 2017-12-09.
  */
object mdpTest1 extends App {
  val X = 5
  val Y = 5

// val env:Environment[DenseMatrix[gridWorldState], gridWorldAction, gridWorldState] = new Environment[DenseMatrix[gridWorldState], gridWorldAction, gridWorldState] {
//     val stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
//      (i,j) => new gridWorldState((i,j), 0)
//    }
//    val allActions:Seq[gridWorldAction] = Seq(North, East, South, West)
//  }
  implicit object gridWorldEnv extends Environment[ gridWorldAction, gridWorldState]{
    def stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
           (i,j) => new gridWorldState((i,j), 0.0)
    }
    def allActions:Seq[gridWorldAction]= Seq(North, East, South, West)
  }
  implicit val policy:gridWorldPolicy = new gridWorldPolicy {
    override def reward(state:gridWorldState, action:gridWorldAction):(gridWorldState, Double) = {
      val A = (0, 1)
      val PRIMEA = (4, 1)
      val B = (0, 3)
      val PRIMEB = (2, 3)
      val XSIZE = X-1
      val YSIZE = Y-1
      val r = (action, state.id) match  {
        case (_, A) => (PRIMEA, 10)
        case (_, B) => (PRIMEB, 5)
        case (North, (_, 0)) => (state.id, -1)
        case (North, b) => ((b._1, b._2-1), 0)
        case (East, (YSIZE, _)) => (state.id, -1)
        case (East, b) => ((b._1+1, b._2), 0)
        case (South, (_, XSIZE)) => (state.id, -1)
        case (South, b) => ((b._1, b._2+1), 0)
        case (West, (0, _)) => (state.id, -1)
        case (West, b) => ((state.id._1-1, state.id._2 ), 0)
        case (_, _) => (state.id, 0)
      }
      (new gridWorldState(r._1,state.value), r._2)
    }
  }
  import rl.experiment.mdp.core.ValueFunctions.Bellman
  Bellman.setDiscount(0.9)
  val result = gridWorldAgent.setEpoch(1000).observe


  println(result.map(a => rounded(3, a.value)))



}
