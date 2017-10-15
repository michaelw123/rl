package rl.tictactoe
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import breeze.linalg._
import rl.tictactoe.game.ROWCOL

import scala.collection.{immutable, mutable}
import scala.util.Random

/**
  * Created by wangmich on 09/19/2017.
  */
object TicTacToe extends App {
  game train
 // game play
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
  def winner:Int = {
    var w:Int = winner(data)
    if (w==0) w = winner(data.t)
    if (w==0) {
      val x = trace(data)
      val y = trace(data.t)

      if (x == data.rows || y == data.rows)
        w = 1
      else if (x == -data.rows || y == -data.rows)
        w = -1
    }
    w
  }
  def winner(d:DenseMatrix[Int]): Int ={
    val rows = d(*, ::)
    for (row <- rows) {
      val thesum = sum(row)
      if (math.abs(thesum) == 3) {
         return math.signum(thesum)
      }
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
  def isTie:Boolean = {
    data.toArray.count(_ == 0) == 0 && winner == 0
  }
}
sealed class Player (val playerSymbol:Int, val exploreRate:Int){
  val stepsize=0.1
  val estimations = mutable.HashMap.empty[Int, Double]
  val states = mutable.LinkedHashMap.empty[Int, State]

  def feedState(state:State): Unit = {
    states.put(state.hashCode, state)
  }
  def feedReward(reward:Double):Unit = {
    if (!states.isEmpty) {
     val key = states.keySet.last
     if (estimations.contains(key)) {
        estimations(key) = estimations(key) + stepsize * (reward - estimations(key))
     } else {
        estimations.put(key, reward)
     }
     feedReward(estimations(key), states.dropRight(1))
   }
  }
  def feedReward(reward:Double, theStates:mutable.LinkedHashMap[Int, State]):Unit = {
    if (!theStates.isEmpty) {
      val key = theStates.keySet.last
      if (estimations.contains(key)) {
        estimations(key) = estimations(key) + stepsize * (reward - estimations(key))
      } else {
        estimations.put(key, reward)
      }
      feedReward(estimations(key), theStates.dropRight(1))
    }
  }

  def takeAction = {
      val (i, j) = nextPosition
      val nextState = nextState1(i,j)
      states.put(nextState.hashCode, nextState)
      (i, j, nextState)
  }
   def nextState1(i:Int,j:Int):State = {
    if (states.isEmpty){
      val newData = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)
      newData(i,j) = playerSymbol
      State(newData)
    } else {
      states.values.last.nextState(i, j, playerSymbol)
    }
  }
   def toExplore:Boolean = exploreRate match {
       case 0 => false
       case _: Int => Binomial(100, exploreRate).draw <= 100 * exploreRate
   }
  private def nextPosition:(Int, Int) = toExplore match {
      case true => explore
      case _ => exploite
  }
  private def explore:(Int, Int) = {
    val latestStateData:DenseMatrix[Int] = {
      if (states.isEmpty) DenseMatrix.zeros[Int](ROWCOL, ROWCOL) else states.values.last.data
    }
    val a = latestStateData.toArray.count(_ == 0)
    if (a==0) {
      println("a is zero!!!")
    }
    val index = Random.nextInt(a)
    var x = 0
//    a.foldLeft(0)((c,d) => {
//      if (d==0) {
//        x=x+1
//      }
//      if (x == index) {
//        return latestStateData.rowColumnFromLinearIndex(c)
//      }
//      c+1
//    })
//    var c=0
//    a.foreach( x match {
//      case 0 => c+1; if (c==index) return latestStateData.rowColumnFromLinearIndex(c)
//      case _ => c+1
//    })

    for (i <- 0 to ROWCOL * ROWCOL -1) {
      if (latestStateData.valueAt(i) == 0) {
        if (x == index) {
          return latestStateData.rowColumnFromLinearIndex(i)
        }
        x = x + 1
      }
    }
    (0, 0)
  }
  private def exploite:(Int, Int) = {
    val availablePositions = mutable.LinkedHashMap[Int, Double]()
    val data = states.values.last.data
    val latestStateData = data.toArray

    for (i <- 0 to ROWCOL * ROWCOL -1) {
      if (latestStateData(i) == 0) {
        val newData = states.values.last.data.copy
        newData(data.rowColumnFromLinearIndex(i))=playerSymbol
        val newState=State(newData)
        val hash = newState.hashCode()
        if (estimations.contains(hash)) {
          availablePositions.put(i, estimations(hash))
        } else {
          availablePositions.put(i, 0)
        }
      }
    }
    val max = availablePositions.maxBy(_._2)
    println("max ="+max)
    data.rowColumnFromLinearIndex(max._1)
  }
  def isTie:Boolean = {
    states.values.last.isTie
  }
  def show = println(states.values.last.data)
  def savePolicy(file:String) = {
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(estimations)
    oos.close
  }
  def loadPolicy(file:String)= {
    val ois = new ObjectInputStream(new FileInputStream(file))
    estimations ++= ois.readObject.asInstanceOf[mutable.HashMap[Int, Double]]
    ois.close

  }
}
object Player {
   case object ai1 extends Player(1, 1)
   case object ai2 extends Player(-1, 1)
   case object ai3 extends Player(1, 0)
   case object human extends Player(-1, 0) {
     override def takeAction = {
       show
       println("enter the position:")
       val input:Int = scala.io.StdIn.readLine().toInt
       println(input)
       val (i,j) = states.values.last.data.rowColumnFromLinearIndex(input)
       val nextState = nextState1(i,j)

       states.put(nextState.hashCode, nextState)
       (i, j, nextState)
     }
     override def feedReward(reward:Double, theStates:mutable.LinkedHashMap[Int, State]): Unit = {
     }
     override def feedReward(reward:Double): Unit = {
     }
   }
}
object game {
  final val ROWCOL = 3
  def findRewards(p1:Player, p2:Player, theWinner:Int) = {
    if (p1.playerSymbol == theWinner) {
      (1.0, 0.0)
    } else if (p2.playerSymbol == theWinner) {
      (0.0, 1.0)
    } else {
      (0.1, 0.5)
    }
  }
  def loadPolicy(file:String): mutable.HashMap[Int, Double]= {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val policy = ois.readObject.asInstanceOf[mutable.HashMap[Int, Double]]
    ois.close
    policy
  }
  def go(p1:Player, p2:Player):Unit = {
    val (i, j, state) = p1.takeAction
    p1.feedState(state)
    p2.feedState(state)
    val winner = state.winner
    if (winner!=0)  {
      val (reward, otherReward) = findRewards(p1, p2, winner)
      p1.feedReward(reward)
      p2.feedReward(otherReward)
      println("found winner:"+winner)
      p1.show
    } else {
      val tie = p1.isTie
      if (!p1.isTie) {
        go(p2, p1)
      }
    }
  }
  def train(implicit data: DenseMatrix[Int] = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)) = {
    val player = Player.ai1
    val otherPlayer = Player.ai2
    for (i <- 0 to 50000) {
      player.states.dropWhile(_ => true)
      otherPlayer.states.dropWhile(_ => true)
      go(player, otherPlayer)
      System.out.println("Epoch "+i)
    }
    println(player.estimations)
    player savePolicy "c://work/tmp/player1-policy"
    println(otherPlayer.estimations)
    otherPlayer savePolicy "c://work/tmp/player2-policy"
  }
  def play(implicit data: DenseMatrix[Int] = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)) = {
    val player = Player.ai3
    player loadPolicy "c://work/tmp/player1-policy"
    val otherPlayer = Player.human
    player.states.dropWhile(_ => true)
    go(player, otherPlayer)
  }
}