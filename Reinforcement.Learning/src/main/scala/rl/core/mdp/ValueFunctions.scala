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
package rl.core.mdp

/**
  * Created by MichaelXiaoqun on 2017-12-09.
  */

object ValueFunctions {

  implicit object Bellman extends ValueFunction {


    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      (statevalue + prob * (reward + getDiscount * nextStateValue))
    }

    override def value[ID](state:State[ID], vrp:Seq[(Double, Double, Double)]): Double = {
      vrp.foldLeft(state.value)((a,b) => a + b._3 * (b._2 + getDiscount * b._1))
    }
    override def qValue[ID](state:State[ID], action:Action):Double = {
        0.0
    }
    def vFunction[ID](state:State[ID]): Double = {
      0.0
    }
  }

  implicit object optimalValueIteration extends ValueFunction {
    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      reward + discount * nextStateValue
    }
    def value[ID](state:State[ID], vrp:Seq[(Double, Double, Double)]): Double = {
      vrp.map(b => b._2 + discount * b._1) max
    }
 }
  implicit object qlearning extends ValueFunction {
    private var learningRate = 0.5
    def setLearningRate(value:Double):this.type = {
      learningRate = value
      this
    }
    override def value(statevalue: Double, nextStateValue: Double, reward: Double, prob: Double): Double = {
      statevalue *(1 -learningRate)+ learningRate *(reward + discount * nextStateValue)
    }
    override def value[ID](state:State[ID], vrp:Seq[(Double, Double, Double)]): Double = {
      vrp.isEmpty match {
        case true => 0
        case _ => {
          val nextValueMax = vrp.map(a => a._1).max
          vrp.map( x =>  state.value * (1 - learningRate) + learningRate *( x._2 + discount*nextValueMax)).max
        }
      }
    }
  }
}


