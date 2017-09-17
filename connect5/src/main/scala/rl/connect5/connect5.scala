package rl.connect5


import breeze.linalg._
/**
  * Created by MichaelXiaoqun on 2017-09-16.
  */


object connect5 extends App{

  val data = DenseMatrix.zeros[Int](5, 5)
 // println(data)

 //val s=State(0, 0, false, DenseMatrix.zeros[Int](10, 10))
  val s=State()
  val s1=s.copy(end=true)
  print(s.winner)
  (0,0,true,DenseMatrix.zeros[Int](10, 10))


}

case class State(winner:Int=0,  var hashVal:Int =0,  end:Boolean = false,  data:DenseMatrix[Int]=DenseMatrix.zeros[Int](10, 10)) {

  override def hashCode: Int = {
    if (hashVal == 0 ) {
      for( i <- data.reshape(data.rows,data.cols)) {
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
