package rl.tictactoe
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import breeze.linalg._

import scala.collection.mutable
/**
  * Created by wangmich on 09/19/2017.
  */
object TicTacToe extends App {
//  val est=new Estimations()
//  val oos = new ObjectOutputStream(new FileOutputStream("/tmp/nflx"))
//  oos.writeObject(est)
//  oos.close
//
//  val ois = new ObjectInputStream(new FileInputStream("/tmp/nflx"))
//  val est1 = ois.readObject.asInstanceOf[Estimations]
//  ois.close

//  println(est1.data)

  game.play


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
  lazy val estimate:Double = {
    val w = winner
    if (w != 0) w
    else 0.5
  }

  def winner:Int = {
    var w:Int = winner(data)
    if (w==0) w = winner(data.t)
    if (w==0) {
      val x = data(0, 0) + data(1, 1) + data(2, 2)
      val y = data(2, 0) + data(1, 1) + data(0, 2)
      println(x, y)
      if (x == data.rows || y == data.rows)
        w = 1
      else if (x == -data.rows || y == -data.rows)
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

sealed trait Player {
  val estimations = mutable.HashMap[Int, Double]()
  val states = mutable.HashMap[Int, State]()

  def feedState(state:State): Unit = {
    states.put(state.hashCode, state)
  }
  def feedReward(reward:Double):Unit = {
    val keys=states.keySet
  }
//  def takeAction:Unit
}
object Player {
  case object AIPlayer extends Player
  case object HumanPlayer extends Player
}
object game {
  final val ROWCOL = 3

  //val data = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)
  def play(implicit data: DenseMatrix[Int] = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)) = {
    data.update(0, 0, -1)
    data.update(1, 1, 0)
    data.update(2, 2, -1)
    val s = State(data)
    val s1 = s.nextState(1, 0, 1)
    val allStates = mutable.HashMap[Int, State]()
   // allStates += ((s.hashCode, s), (s1.hashCode, s1))
    //println(allStates)
  }
}
@SerialVersionUID(123L)
class Estimations extends Serializable {
  val data: mutable.HashMap[Int, Double] = mutable.HashMap((1, 2.3), (2, 4.5))
}