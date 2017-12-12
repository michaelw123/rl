package test.mdp

import rl.experiment.mdp.GridWorld.gridWorldAction._
import rl.experiment.mdp.GridWorld._
import rl.experiment.mdp.core._
import rl.experiment.mdp.core.ValueFunctions.Bellman
import breeze.linalg.DenseMatrix

/**
  * Created by Michael Wang on 2017-12-09.
  */
object mdpTest1 extends App {
  val X = 10
  val Y = 10

// val env:Environment[DenseMatrix[gridWorldState], gridWorldAction, gridWorldState] = new Environment[DenseMatrix[gridWorldState], gridWorldAction, gridWorldState] {
//     val stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
//      (i,j) => new gridWorldState((i,j), 0)
//    }
//    val allActions:Seq[gridWorldAction] = Seq(North, East, South, West)
//  }
  implicit object gridWorldEnv extends Environment[ gridWorldAction, gridWorldState]{
    val stateSpace:DenseMatrix[gridWorldState] = DenseMatrix.tabulate[gridWorldState](X,Y){
           (i,j) => new gridWorldState((i,j), 0)
    }
    val allActions:Seq[gridWorldAction]= Seq(North, East, South, West)
  }
  implicit val policy:gridWorldPolicy = new gridWorldPolicy {
    override def reward(state:gridWorldState, action:gridWorldAction):(gridWorldState, Double) = {
      val A = (0, 1)
      val PRIMEA = (4, 1)
      val B = (0, 3)
      val PRIMEB = (2, 3)
      val r = (action, state.id) match  {
        case (_, A) => (PRIMEA, 10)
        case (_, B) => (PRIMEB, 5)
        case (North, (0, _)) => (state.id, -1)
        case (North, b) => ((state.id._1 - 1, state.id._2), 0)
        case (East, (_, Y)) => (state.id, -1)
        case (East, b) => ((state.id._1, state.id._2 + 1), 0)
        case (South, (X, _)) => (state.id, -1)
        case (South, b) => ((state.id._1 + 1, state.id._2), 0)
        case (West, (_, 0)) => (state.id, -1)
        case (West, b) => ((state.id._1, state.id._2 - 1), 0)
        case (_, _) => (state.id, 0)
      }
      (new gridWorldState(r._1, 0), r._2)
    }
  }

  val result = gridWorldAgent.observe


  println(result.map(a => a.value))



}
