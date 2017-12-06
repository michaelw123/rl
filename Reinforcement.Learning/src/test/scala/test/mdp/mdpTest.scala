package test.mdp

import rl.mdp.GridWorldMDP._

/**
  * Created by wangmich on 12/06/2017.
  */
object mdpTest extends App{

  val state = new gridWorldState(0,1)
  val aa = gridWorldReward(9.0)

  val qq = aa match {
    case  gridWorldReward(9.1) => "match"
    case _ => "not match"
  }
  //  println(yy)
  //  println(xx)
  println(aa)
  println(qq)
  val p = gridWorldPolicy.pi(new gridWorldState(0,1), North)
  println(p)

  val allstates = gridWorldStates.allStates

  println(allstates.map(a => (a.x, a.y)))
  val aState = new gridWorldState(10,19)
  println(aState.availableActions)


}
