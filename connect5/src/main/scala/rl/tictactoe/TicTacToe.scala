package rl.tictactoe
import breeze.linalg._
import scala.collection.mutable.HashMap
/**
  * Created by wangmich on 09/19/2017.
  */
object TicTacToe extends App {
  val data = DenseMatrix.zeros[Int](3, 3)
  data(0,0)= -1
  data(1,1)= 0
  data(2,2)= -1
  val s= State(data)
  val s1=s.nextState(1,0, 1)
  val allStates = HashMap[Int, State]()
  allStates += ((s.hashCode, s), (s1.hashCode, s1))
println(allStates)

  //s.show
  //s.winner

  //s1.show
 // s1.winner

//  println(s.hashVal)
//  println("winner is:" +s.winner)
//  println(s.isEnd)

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
  lazy val isEnd:Boolean = winner!=0

  def winner:Int = {
    var w:Int = winner(data)
    if (w==0) w = winner(data.t)
    if (w==0) {
      val x = data(0, 0) + data(1, 1) + data(2, 2)
      val y = data(2, 0) + data(1, 1) + data(0, 2)
      println(x, y)
      if (x == 3 || y == 3)
        w = 1
      else if (x == -3 || y == -3)
        w = -1
    }
    w
  }
  def winner(d:DenseMatrix[Int]): Int ={
    val rows = data(*, ::)
    for (row <- rows) {
      if (sum(row) == 3)
        return 1
      else if (sum(row) == -3)
        return -1
    }
    0
  }
  def nextState(i:Int, j:Int, player:Int): State = {
    val newData = data.copy
    newData(i,j) = player
    State(newData)
  }
  def show = {
    println(data)
  }
}

trait Player {

}
