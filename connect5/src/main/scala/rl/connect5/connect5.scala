package rl.connect5
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import breeze.linalg._
import rl.connect5.game.ROWCOL

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.util.Random

/**
  * Created by Michael Wang on 09/19/2017.
  */
object connect5 extends App {
  //game train
  //game play
  game test
}
import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Binomial

case class State( data:DenseMatrix[Int]) {
  val hashVal:Int = hashCode
  override def hashCode: Int = hashVal match {
    case 0 => data.toArray.foldLeft(0)(_ * 3 + _ + 1)
    case _ => hashVal
  }
  def winner:Int = {
    var w: Int = winner(data)
    if (w == 0) w = winner(rot90(data))
    if (w ==0) w = diagWinner(data)
    w
  }
  def winner(d:DenseMatrix[Int]): Int ={
    val rows = d(*, ::)
    for (row <- rows) {
      val w = check5(row)
      if (w!=0) return w
    }
    0
  }
  private def check5(v:DenseVector[Int]):Int = {
    v.foldLeft(0)((a, b) => {
      if (math.signum(a) != math.signum(b)) b
      if (math.abs(a) ==game.FIVE-1 ) return math.signum(a)
      a+b
    })
    0
  }
  def diagWinner(m:DenseMatrix[Int]): Int = {
    if (m.rows == 5) {
       diagWinner5(m)
    }
    val m1 = m(0 to m.rows-2,0 to m.rows-2)
    val m2 = m(0 to m.rows-2, 1 to m.rows-1)
    val m3 = m(1 to m.rows-1, 0 to m.rows-2)
    val m4 = m(1 to m.rows-1, 1 to m.rows-1)
    val x = Seq(m1, m2, m3, m4).foldLeft(0){
      (a, b) => {
        if (a == 0) {
          val x = diagWinner(b)
          if (math.abs(x) == 5) {
            math.signum(x)
          } else {
            0
          }
        } else {
          a
        }
      }
    }
    x
  }
  def diagWinner5(m:DenseMatrix[Int]): Int = {
    val x = trace(m)
    val y = trace(rot90(m))
    val result = (x,y) match {
      case (5, _) => 1
      case (-5, _) => -1
      case (_, 5) => 1
      case (_, -5) => -1
      case _ => 0
    }
    result
  }
  def nextState(i:Int, j:Int, player:Int): State = {
    val newData = data.copy
    newData(i,j) = player
    State(newData)
  }
  def show = println(data)
  def isTie:Boolean = isEnd && (winner == 0)
  def isEnd:Boolean = data.toArray.count(_ == 0) == 0
}
sealed class Player (val playerSymbol:Int, val exploreRate:Int){
  val stepsize=0.1
  val estimations = mutable.HashMap.empty[Int, Double]
  val states = mutable.LinkedHashMap.empty[Int, State]

  def currentData = states.isEmpty match {
    case true => DenseMatrix.zeros[Int](game.ROWCOL, game.ROWCOL)
    case _ => currentState.data
  }

  def currentState = states.isEmpty match {
    case true => State(DenseMatrix.zeros[Int](game.ROWCOL, game.ROWCOL))
    case false => states.values.last
  }
  def feedState(state:State): Unit = {
    states.put(state.hashCode, state)
  }
  def feedReward(reward:Double, theStates:mutable.LinkedHashMap[Int, State]):Unit = {
    if (!theStates.isEmpty) {
      val (key, theState) = theStates.last
      if (!estimations.contains(key)) {
        estimations.put(key, (theState.isEnd, theState.winner) match {
          case (true, playerSymbol) => 1
          case (true, _) => 0
          case (fasle, _) => 0.5
        })
      }
      estimations(key) = estimations(key) + stepsize * (reward - estimations(key))
      feedReward(estimations(key), theStates.dropRight(1))
    }
  }
  def takeAction = {
    val (i, j) = nextPosition
    val nState = nextState(i,j)
    states.put(nState.hashCode, nState)
    (i, j, nState)
  }
  def nextState(i:Int,j:Int):State = states.isEmpty match {
    case true => {
      val newData = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)
      newData(i,j) = playerSymbol
      State(newData)
    }
    case _ => currentState.nextState(i, j, playerSymbol)
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
    val a = currentData.toArray.count(_ == 0)
    val index = Random.nextInt(a)
    var x = 0
    for (i <- 0 to ROWCOL * ROWCOL -1 if currentData.valueAt(i) == 0) {
      if (x == index) {
        return currentData.rowColumnFromLinearIndex(i)
      }
      x = x + 1
    }
    currentData.rowColumnFromLinearIndex(x)
  }
  private def exploite:(Int, Int) = {
    val availablePositions = mutable.LinkedHashMap[Int, Double]()
    val data = currentData
    val latestStateData = data.toArray
    for (i <- 0 to ROWCOL * ROWCOL -1 if latestStateData(i) == 0) {
      val newData = data.copy
      newData(data.rowColumnFromLinearIndex(i))=playerSymbol
      val newState=State(newData)
      val hash = newState.hashVal
      availablePositions.put(i, estimations.contains(hash) match {
        case true => estimations(hash)
        case _ => 0
      })
    }
    val max = availablePositions.maxBy(_._2)
    data.rowColumnFromLinearIndex(max._1)
  }
  def isTie:Boolean = {
    currentState.isTie
  }
  def isEnd:Boolean = {
    currentState.isEnd
  }
  def show = println(currentData)
  def savePolicy(file:String) = {
    println("estimations size:"+estimations.size)
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
      val (i,j) = currentData.rowColumnFromLinearIndex(input)
      val nState = nextState(i,j)
      states.put(nState.hashCode, nState)
      (i, j, nState)
    }
    override def feedReward(reward:Double, theStates:mutable.LinkedHashMap[Int, State]): Unit = {
    }
  }
}
object game {
  final val ROWCOL = 10
  final val FIVE=5
  def findRewards(p1:Player, p2:Player, theWinner:Int) = {
    theWinner match {
      case p1.playerSymbol => (1.0, 0.0)
      case p2.playerSymbol => (0.0, 1.0)
      case _ => (0.5, 0.5)
    }
  }
  def loadPolicy(file:String): mutable.HashMap[Int, Double]= {
    val ois = new ObjectInputStream(new FileInputStream(file))
    val policy = ois.readObject.asInstanceOf[mutable.HashMap[Int, Double]]
    ois.close
    policy
  }
  @tailrec
  def go(p1:Player, p2:Player):Unit = {
    val (i, j, state) = p1.takeAction
    p1.feedState(state)
    p2.feedState(state)
    val winner = state.winner
    winner match {
      case 0 => if (!p1.isEnd) go(p2, p1)
      case _ => val (reward, otherReward) = findRewards(p1, p2, winner)
        p1.feedReward(reward, p1.states)
        p2.feedReward(otherReward, p2.states)
        println("found winner:"+winner)
        p1.show
    }
  }
  def train(implicit data: DenseMatrix[Int] = DenseMatrix.zeros[Int](ROWCOL, ROWCOL)) = {
    val player = Player.ai1
    val otherPlayer = Player.ai2
    for (i <- 0 to 400000) {
      player.states.clear
      otherPlayer.states.clear
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
    player.states.clear
    player loadPolicy "c://work/tmp/player1-policy"
    val otherPlayer = Player.human
    otherPlayer.states.clear
    go(player, otherPlayer)
  }
  def test = {
    //val m: DenseMatrix[Int] = DenseMatrix.zeros[Int](5,5)
    val m = DenseMatrix((-1,1,0,1,1, 0), (1,-1,-1,-1, -1,0),(1, -1,-1,1,-1,0), (1, 0, 0, -1, -1,0), (1, 0, 0, -1, -1,0), (1, 0, 0, -1, -1,0))
    val s = State(m)
    println(s.winner)
    s.show
  }
}