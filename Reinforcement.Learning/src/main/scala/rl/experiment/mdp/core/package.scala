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
  trait Environment [S]{
    def stateSpace:DenseMatrix[S]
    var result:DenseMatrix[S]
    def allActions:Seq[Action]
    def setResult(value :DenseMatrix[S]) = result = value
  }
  trait Agent[A, S] {
    //def setEnvironment(env:Environment[A, S, CS[S]]):this.type
   def observe[VF <: ValueFunction,  P <:Policy[S, A],  E <: Environment[S]](implicit vf: VF, policy:P, env:E):DenseMatrix[S]
   // def test[VF <: ValueFunction, P <: Policy[S, A]](a:A, s:S)(implicit vf:VF, p:P) :Double
  }

  trait ValueFunction{
    def setDiscount(value:Double): this.type
    def getDiscount:Double
    def value(statevalue:Double, nextStateValue:Double, reward:Double, prob:Double):Double
   // def value[ID, P](state:State[ID])(implicit policy:P):Double
    //def value[ID, P](state:State[ID])(implicit policy:P, env:Environment[State[ID]]):Unit
  }
}
