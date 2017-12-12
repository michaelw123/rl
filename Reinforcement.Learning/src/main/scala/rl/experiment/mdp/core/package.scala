package rl.experiment.mdp

import breeze.linalg.DenseMatrix
import rl.experiment.mdp.core.Policy

/**
  * Created by Michael Wang on 2017-12-09.
  */
/*
Type Notations:
S: State
E: Environment
A: Action
CS: Container of States, most likely, DenseVector or DenseMatrix from breeze
VF: ValueFunction
For simplicity, reward and value are Double, and the container of actions are Seq
The id of the state depends on the container used, for instance, (Int, Int) for matrix, Int for scalar container
Note: policy/action/state belong to problems to be solved, such as GridWorld problem, while a value function belongs to an algorithm, such as Bellman
equation. Agent glues them together.
ValueFunction Environment belongs to the valueFunction that defines the attributes of the algorithm, i.e., Discount
Problem Environment belongs to the Problem that defines state space, etc

 */
package object core {
  trait Action
  trait State[ID] {
    val id:ID
    var value:Double
  }
  trait Policy[S, A]{
    def reward(state:S, action:A):(S, Double)
    def availableActions(state:S):Seq[A]
    def getActionProb(action:A):Double
  }
  trait Environment[A, S] {
    val stateSpace:DenseMatrix[S]
    val allActions:Seq[A]
  }
  trait Agent[A, S] {
    //def setEnvironment(env:Environment[A, S, CS[S]]):this.type
    def observe[VF <: ValueFunction,  P <:Policy[S, A],  E <: Environment[A, S]](implicit vf: VF, policy:P, env:E):DenseMatrix[S]
  }

  trait ValueFunction{
    def setDiscount(value:Double): this.type
    def getDiscount:Double
    def value(statevalue:Double, nextStateValue:Double, reward:Double, prob:Double):Double
  }
}
