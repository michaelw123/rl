package test.mdp

import rl.utils.poisson

/**
  * Created by wangmich on 12/21/2017.
  */
object test1 extends App {
  var prob = 0.0
  for (i <- 0 until scala.math.min(10, 6)) {
    val p = poisson(3, i) * poisson(3, scala.math.abs(0 -i))
    prob += p
    println(i, p, prob)
  }
  println(prob)

  println(poisson(3,5))
}
