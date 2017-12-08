package rl

/**
  * Created by wangmich on 12/08/2017.
  */
package object utils {
  def rounded(x: Int, n:Double) = {
    val w = math.pow(10, x)
    (n * w).toLong.toDouble / w
  }
}
