/*
 * Copyright (c) 2017 Michael Wang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package rl.core

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
Problem Environment belongs to the Problem that defines state space and action space, etc.

 */
package object mdp {
  trait Action {
    val value = 0
  }
  trait State[ID] {
    val id:ID
    var value:Double
  }
  trait Policy[S, A] {
    def bestAction(state:S):A
  }


  trait Environment [CS[_], S, A]{
    def stateSpace:CS[S]
    var currentStates = None: Option[CS[S]]
    def actionSpace:Seq[A]
    def update(value :CS[S]) = currentStates = Option(value)
    def reward(state:S, action:A):(S, Double) // an action takes S to S' deterministically
    def reward(state:S, action:A, nextState:S):Double // an action may take S to multiple S', propability is given by transactionProb, this reward function calculates the transaction R(S, A, S')
    def transitionProb(state:S, action:A, nextState:S):Double //transaction probability
    def transitionRewardProb(state:S, action:A, nextState:S):(Double, Double) = ??? // return (prob, reward) pair
    def cost(state:S, action:A):Double  //if the destination state is deterministic by an action
    def cost(state:S, action:A, nextState:S):Double //an action may take S to multiple S', propability is given by transactionProb, this cost function calculates the transaction Cost(S, A, S')
    def availableTransitions(state:S):Seq[(A, S)]
    def availableActions(state:S):Seq[A]
    def getCurrentStates:CS[S] = {
      if (!currentStates.isDefined) currentStates=Option(stateSpace)
      currentStates.get
    }
  }
  trait Agent[A, CS[_], S] {
   def observe[VF <: ValueFunction,  P <:Policy[S, A],  E <: Environment[CS, S, A]](implicit vf: VF, policy:P, env:E):CS[S]
  }

  trait ValueFunction{
    def setDiscount(value:Double): this.type
    def getDiscount:Double
    def value(statevalue:Double, nextStateValue:Double, reward:Double, prob:Double):Double
    def value[ID](state:State[ID], vrp:Seq[(Double, Double, Double)]): Double // next state value, reward, action probability
  }
}
