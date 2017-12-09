package rl.experiment.mdp

/**
  * Created by Michael Wang on 2017-12-09.
  */
/*
Type Notations:
S: State
E: Environment
A: Action
CS: Container of States, most likely, DenseVector or DenseMatrix from breeze
For simplicity, reward and value are Double, and the container of actions are Seq
The id of the state depends on the container used, for instance, (Int, Int) for matrix, Int for scalar container
Note: policy/action/state belong to problems to be solved, such as GridWorld problem, while a value function belongs to an algorithm, such as Bellman
equation. Agent glues them together.
ValueFunction Environment belongs to the valueFunction that defines the attributes of the algorithm, i.e., Discount
Problem Environment belongs to the Problem that defines state space, etc

 */
package object core {
  trait Action
  trait State {
    type I
    val id:I
    var value:Double
  }

  trait Agent[S, E] {
    def run(state:S) (implicit policy;T):Double = vf.value(env, state)
  }
  trait Policy[S, A]{
    def reward(state:S, action:A):(S, Double)
    def availableActions(state:S):Seq[A]
  }

  trait ValueFunction[E, S] {
    def value(env:E, state:S):Double
  }
  trait Environment[A, S, CS[_]] {
    val allStates:CS[S]
    val allActions:Seq[A]
  }
}
