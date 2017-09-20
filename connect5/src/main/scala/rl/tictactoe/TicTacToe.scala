package rl.tictactoe
import breeze.linalg._
/**
  * Created by wangmich on 09/19/2017.
  */
object TicTacToe extends App {
  val DIMENSION = 3

  val data = DenseMatrix.zeros[Int](DIMENSION, DIMENSION)
  data(0,0)=1
  data(0,1)=1
  data(0,2)=1
  val s= State(data)
  println(s.hashVal)
  println("winner is:" +s.winner)
  println(s.isEnd)

}
import breeze.linalg.DenseMatrix

case class State( data:DenseMatrix[Int]) {
    val hashVal:Int = hashCode
    override def hashCode: Int = {
      if (hashVal == 0 ) {
        data.toArray.foldLeft(0)(_*3 + _+1)
      } else {
        hashVal
      }
   }
  def isEnd:Boolean = winner!=0

  def winner:Int = {
    val rows = data(*, ::)
    for (row <- rows) {
      println(sum(row))
     if (sum(row) == 3)
       return 1
     else if (sum(row) == -3)
       return -1
    }
    val cols = data(::, *)
    for (col <- cols) {
      println(sum(col))
      if (sum(col) == 3)
        return 1
      else if (sum(col) == -3)
        return -1
    }
    val x = data(0,0) + data(1,1)+ data(2, 2)
    val y = data(2,0) + data(1,1)+ data(0, 2)
    println(x, y)
    if (x == 3 || y == 3)
      return 1
    else if (x == -3 || y == -3)
      return -1
     0
  }
}
