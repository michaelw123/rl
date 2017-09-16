package rl.connect5


import breeze.linalg._
/**
  * Created by MichaelXiaoqun on 2017-09-16.
  */


object connect5 extends App{
  println("hello")

  val data = DenseMatrix.zeros(10, 10)
}
case class State(winner:Int=0, var hashVal:Int =0, end:Boolean = false, data:DenseMatrix[Int]=DenseMatrix.zeros(10, 10)) {

  def getHash: Int = {
    if (hashVal == 0 ) {
      for( i <- data.reshape(10,10)) {
        hashVal = hashVal *3 + i
      }
    }
    hashVal
  }
//  def isEnd:Boolean = {
//    checkColumn && checkRow && checkDiagnol
//  }
//  def checkColumn:Boolean = {
//    for (rows <- data.rows  ) {
//      rows.
//    }
//  }
}
