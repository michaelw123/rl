package rl.experiment.rl


/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */
package object core {
  trait Action
  trait State {
    type I = (Int, Int)
    val id:I
    var value:Double
  }

  trait Agent[S, V] {
    def value[T](config:T) (implicit vf:ValueFunction[T, S])
  }
  trait Policy[S, A, E]{
    def reward(state:S, action:A):(S, Double)
    def availableActions(state:S):Seq[A]
  }

  trait ValueFunction[C, S] {
    def value(config:C, state:S):Double
  }
  trait Environment[A, S, CS[_]] {
    val allStates:CS[S]
    val allActions:Seq[A]
  }
}
