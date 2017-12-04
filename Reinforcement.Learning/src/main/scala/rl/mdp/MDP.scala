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

package rl.mdp
import breeze.stats.distributions.Rand

/**
  * Created by Michael Wang on 2017-12-03.
  */
object MDP {

  trait Action

  trait Value

  trait Reward

  trait Statable[S] {
    def transition(state: S, action: Action): (S, Reward)

    def availableActions(state: S): Seq[Action]
    def allActions: IndexedSeq[Action]
    def allStates:IndexedSeq[S]
  }

  trait Valuable[S] extends Statable[S] {
    def value(state: S): Value

    def heuristic(state: S): Value
  }

  trait Randomizable[S] extends Valuable[S] {
    def genRandom(): S
  }

    implicit class StatableOps[S: Statable](state: S) {
      val func = implicitly[Statable[S]]
      def availableActions: Seq[Action] = func.availableActions(state)
      def randomAction: Action = Rand.choose(availableActions).draw
      def applyTransition(action: Action) = func.transition(state, action)
    }

    trait Policy[G[_] <: Statable[_]]{
      def nextAction[S: G](s: S): Action
      def applyNext[S: G: Statable](s: S): (S, Reward) =
        s.applyTransition(nextAction[S](s))
    }
}

