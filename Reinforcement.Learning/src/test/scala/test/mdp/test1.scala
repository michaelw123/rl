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
package test.mdp

import scala.util.Random
/**
  * Created by Michael Wang on 04/11/2018.
  */

object test extends App {
  var arguments = args map (_.toInt)

  def getPoint(): Int = {
    if (scala.math.sqrt(scala.math.pow(Random.nextDouble,2) + scala.math.pow(Random.nextDouble,2)) <= 1) 1 else 0
  }

  def getRatio(np: Int): Double = {
    (List.fill(np)(getPoint).sum * 1.0) / np
  }

//  var pi = (List.fill(arguments(0))(getRatio(arguments(1))).sum / arguments(0)) * 4.0
  //  Console.println("Approximation of Pi: " + pi)

  val aa = Array.ofDim[Double](10,10,2,4,5)
  type bb = aa.type
  println("bb")

}
