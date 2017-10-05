package rl.tictactoe
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import breeze.linalg._
import rl.tictactoe.game.ROWCOL

import scala.collection.immutable
import scala.collection.mutable
import scala.util.Random

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
//  val player= AIPlayer
//  player.toExplore



}
import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Binomial

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
 //     println(x, y)
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

sealed class Player (val playerSymbol:Int){
  val stepsize=0.1
  val exploreRate = 0.5
  val estimations = mutable.HashMap[Int, Double]()
  val states = mutable.LinkedHashMap[Int, State]()

  def feedState(state:State): Unit = {
    states.put(state.hashCode, state)
    estimations.put(state.hashCode, 0)
  }
  def feedReward(reward:Double, theStates:mutable.LinkedHashMap[Int, State]):Unit = {
    if (theStates.isEmpty) return
    val key=theStates.keySet.last
    estimations(key) = estimations(key) + stepsize * (reward - estimations(key))
    feedReward(estimations(key), theStates.dropRight(key))
  }
  def takeAction = {
    //if (toExplore) {
      val (i, j) = nextPosition
      val nextState = nextState1(i,j)

      states.put(nextState.hashCode, nextState)
      (i, j, nextState)
    //} else {  //to Exploit

    //}
  }
  private def nextState1(i:Int,j:Int):State = {
    if (states.isEmpty){
      val newData = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)
      newData(i,j) = playerSymbol
      State(newData)
    } else {
      states.values.last.nextState(i, j, playerSymbol)
    }
  }
   def toExplore:Boolean = {
    return Binomial(100, exploreRate).draw < 100* exploreRate
  }
  private def nextPosition:(Int, Int) = {
    if (!states.isEmpty) {
      val latestStateData = states.values.last.data
      val a = latestStateData.toArray
      val size = a.filter(_ == 0).size
      val index = Random.nextInt(size)
      var x = 0
      //for ((i, j ) <- (0 to latestStateData.rows-1, 0 to latestStateData.cols-1)) {
//      for (i <- 0 to latestStateData.rows - 1) {
//        for (j <- 0 to latestStateData.cols - 1) {
//          if (latestStateData.valueAt(i, j) == 0) x = x + 1
//          if (x == index) (i, j)
//        }
//      }
      for (i <- 0 to ROWCOL*ROWCOL) {
        if (latestStateData.valueAt(i) == 0) x=x+1
        if (x == index) return latestStateData.rowColumnFromLinearIndex(i)
      }
      (0,0)
    } else {
      ((game.ROWCOL/2) toInt, (game.ROWCOL/2) toInt )
    }
  }
}
object Player {
   case object ai1 extends Player(1)
   case object ai2 extends Player(-1)
   case object human extends Player(-1)
}
object game {
  final val ROWCOL = 3

  //val data = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)
  def play(implicit data: DenseMatrix[Int] = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)) = {
//    data.update(0, 0, -1)
//    data.update(1, 1, 0)
//    data.update(2, 2, -1)
//    val s = State(data)
//    val s1 = s.nextState(1, 0, 1)
//    val allStates = mutable.HashMap[Int, State]()
//   // allStates += ((s.hashCode, s), (s1.hashCode, s1))
//    //println(allStates)
    val allStates:mutable.HashMap[Int, (State, Boolean)]= mutable.HashMap()
    def buildAllStates(currentState:State, playerSymbol:Int):Unit = {
      for (i <- 0 to ROWCOL-1) {
        for (j <- 0 to ROWCOL-1 ) {
          if (currentState.data(i,j)==0) {
            val newState  = currentState.nextState(i, j, playerSymbol)
            val newHash = newState.hashVal
            if (!allStates.contains(newHash)) {
              val isEnd = newState.isEnd
              allStates.put(newHash, (newState, isEnd))
              if (!isEnd) buildAllStates(newState, -playerSymbol)
            }
          }
        }
      }
    }
    val player:Player = Player.ai1
    val otherPlayer:Player = Player.ai2
    def findRewards(p1:Player, p2:Player, theWinner:Int) = {
      if (p1.playerSymbol == theWinner) {
        (1.0, 0.0)
      } else if (p2.playerSymbol == theWinner) {
        (0.0, 1.0)
      } else {
        (0.1, 0.5)
      }
    }
    def go(p1:Player, p2:Player):Unit = {
      val (i, j, state) = p1.takeAction
      p1.feedState(state)
      p2.feedState(state)
      val winner = state.winner
      if (winner!=0)  {
        val (reward, otherReward) = findRewards(p1, p2, winner)
        p1.feedReward(reward, p1.states)
        p2.feedReward(otherReward, p2.states)
        println("found winner:"+winner)
      } else {
        go(p2, p1)
      }
    }
    buildAllStates(State(DenseMatrix.zeros[Int](ROWCOL, ROWCOL)), 1)
    go(player, otherPlayer)

    println(player.estimations)
    println(otherPlayer.estimations)
  }

}
@SerialVersionUID(123L)
class Estimations extends Serializable {
  val data: mutable.HashMap[Int, Double] = mutable.HashMap((1, 2.3), (2, 4.5))
}