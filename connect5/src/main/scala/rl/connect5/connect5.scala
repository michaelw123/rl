package rl.connect5


import breeze.linalg._
/**
  * Created by MichaelXiaoqun on 2017-09-16.
  */


object connect5 extends App{

//  val data = DenseMatrix.zeros[Int](2, 2)
//  data(0,0)=1
//  data(1,1)=2
////  data(2,2)=3
////  data(3,3)=4
////  data(4,4)=5
//
//  val d = data.toArray
//  d.foreach(print _)
//  println
//  val hashVal = d.foldLeft(0)(_*3 + _)
//  println(hashVal)
  val data = DenseMatrix.zeros[Long](8, 8)

  //data(0,0)=1
  data(0,0)=2
//  data(1,2)=1
//  data(2,2)=1
  //data(3,3)=2
  val s=new State(data)
  println(data)
  data.toArray.foreach(print _)
  println
  println(s.hashCode1)
  println(Long.MaxValue)
//  val s=State()
//  val s1=s.copy(end=true)
//  print(s.winner)
 


}

class State(val data:DenseMatrix[Long]) {
  //var hashVal:Int
//  def apply(): Unit ={
//    hashVal = 0
//  }

//  override def hashCode: Int = {
//   // if (hashVal == 0 ) {
//      data.toArray.foldLeft(0)(_*3 + _)
//    //}
//   // hashVal
//  }
  def hashCode1: Long = {
    // if (hashVal == 0 ) {
    data.toArray.foldLeft(0L)(_*3 + _)
    //}
    // hashVal
  }
//  def isEnd:Boolean = {
//    checkColumn && checkRow && checkDiagnol
//  }
//  def checkRow:Boolean = {
//    for (row <- data.rows  ) {
//      println(row)
//    }
//  }



}
