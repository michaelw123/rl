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
    var stochastic=false // deterministic by default
    def setIsStochastic(value:Boolean):this.type = {
      stochastic = value
      this
    }
    def isStochastic = stochastic
    def bestAction(state:S):A
    def applicableActions(state:S):Seq[A]
    def actionProb(state:S, action:A):Double
    def update(state:S, action:A):Unit = ??? //policy iteration
    def isStable:Boolean = false //check if the policy has changed between each iteration
  }


  trait Environment [CS[_], S, A]{
    def stateSpace:CS[S]
    var currentStates = None: Option[CS[S]]
    def actionSpace:Seq[A]
    def update(value :CS[S]) = currentStates = Option(value)
    def reward(state:S, action:A):(S, Double) = ??? // an action takes S to S' deterministically
    def reward(state:S, action:A, nextState:S):Double = ??? // an action may take S to multiple S', propability is given by transitionProb, this reward function calculates the transition R(S, A, S')
    def stochasticRewards(state:S, action:A):Seq[(Double, Double, Double)] = ??? // given a state and an action, returns a sequence of VRP - value of nextState, Reward, and Action Probability,
          // which is applied to Value Functions such as Bellman equation, stochastic
    def transitionProb(state:S, action:A, nextState:S):Double = ??? //transition probability for each action - deterministic
    def transitionRewardProb(state:S, action:A, nextState:S):(Double, Double) = ??? // return (prob, reward) pair, the reward is sum(prob * reward)
    def cost(state:S, action:A):Double = 0  //if the destination state is deterministic by an action
    def cost(state:S, action:A, nextState:S):Double = 0 //an action may take S to multiple S', propability is given by transactionProb, this cost function calculates the transaction Cost(S, A, S')
    def applicableTransitions(state:S):Seq[(A, S)] = ???
    def getCurrentStates:CS[S] = {
      if (!currentStates.isDefined) currentStates=Option(stateSpace)
      currentStates.get
    }
  }
  trait Agent[A, CS[_], S] {
     var policyIteration = false  //policy iteration only applies to stochastic policy
     var valueIteration = false
     var epoch = 1
     var exitDelta=0.0
    var isStochastic = false
    def setIsStochastic(value:Boolean):this.type = {
      isStochastic=value
      this
    }
    def getIsStochastic = isStochastic
    def setExitDelta(value:Double) = {
      exitDelta = value
      this
    }
    def setEpoch(value:Int) : this.type ={
      epoch = value
      this
    }
    def getPolicyIteration = policyIteration
    def setPolicyIteration(value:Boolean):this.type = {
      policyIteration = value
      this
    }
    def getValueIteration = valueIteration
    def setValueIteration(value:Boolean):this.type = {
      valueIteration = value
      this
    }
   def observe[VF <: ValueFunction,  P <:Policy[S, A],  E <: Environment[CS, S, A]](env:E,policy:P) (implicit vf: VF):CS[S]
  }

  trait ValueFunction{

    private var discount = 0.0

    def setDiscount(value: Double): this.type = {
      discount = value
      this
    }

    def getDiscount = discount

    def value(statevalue:Double, nextStateValue:Double, reward:Double, prob:Double):Double
    def value[ID](state:State[ID], vrp:Seq[(Double, Double, Double)]): Double // next state value, reward, action probability
  }
}
