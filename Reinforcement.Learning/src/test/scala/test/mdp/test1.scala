package test.mdp

import rl.core.mdp.GridWorld.{gridWorldAction, gridWorldState}
import rl.utils.poisson
import scala.swing._
import net.ericaro.surfaceplotter.{ProgressiveSurfaceModel, JSurfacePanel, Mapper}

/**
  * Created by wangmich on 12/21/2017.
  */
object test1 extends App {
//  var prob = 0.0
//  for (i <- 0 until scala.math.min(10, 6)) {
//    val p = poisson(3, i) * poisson(3, scala.math.abs(0 -i))
//    prob += p
//    println(i, p, prob)
//  }
//  println(prob)
//
//  println(poisson(3,5))
//  (state:gridWorldState, action:gridWorldAction, nextState:gridWorldState):(Double, Double)

//  val  state:gridWorldState=new gridWorldState((18, 17), 0)
//
//  val actionState = carRentalClient.carRentalEnv.availableTransitions(state)
//  var vrp = Seq[(Double, Double, Double)]()
//  for ((action, nextState) <- actionState) {
//    val (actionProb, reward ) = carRentalClient.carRentalEnv.transitionRewardProb(state, action, nextState)
//    if (reward!=0 && actionProb!=0) {
//      vrp = vrp :+ (nextState.value, reward - carRentalClient.carRentalEnv.cost(state, action, nextState), actionProb)
//    }
//  }
//  println("aaa")
//  /state.value = vf.value(state, vrp)

  //println(poisson(4, 0))

//  val model = new ProgressiveSurfaceModel
//  val surfacePanel = new JSurfacePanel
//  surfacePanel.setModel(model)
//
//  model.setMapper(new Mapper {
//    def f1(x:Float, y:Float) = {
//      val r = x*x + y*y +x/y
//
//      if (r == 0 ) 1f else (Math.sin(r)/r).toFloat
//    }
//
//    def f2(x:Float, y:Float) = {
//      (Math.sin(x*y)).toFloat
//    }
//  })
//
//  model.plot.execute
//
//  new MainFrame {
//    contents = new Component {
//      override lazy val peer = surfacePanel
//    }
//    visible = true
//  }

  //class Storage[DocType](name: String)
//  trait Document1
//
//  class Storage[DocType = Document1](name: String)

}
