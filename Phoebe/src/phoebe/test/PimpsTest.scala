package phoebe.test

import phoebe.Prelude._
import phoebe.godel.EquationSolver

object PimpsTest {
  def main(args: Array[String]) {
    println(EquationSolver.solveQuadratic(1, 0, 1))

    skip {
      2 times {
        println("Hello World!")
      }

      4 times { i =>
        println("Index value: " + i)
      }

      println(4 isOdd, 4 isEven)

      println(List(2, 8, 11).isAscending)

      println(Vector(3, 3, 3))

      time("`for` with 10000 iterations") {
        for(_ <- 1 to 10000) {

        }
      }

      time("`while` with 10000 iterations") {
        var i = 0
        while(i < 10000) {
          i += 1
        }
      }

      time("`cfor` with 10000 iterations") {
        cfor(1, _ <= 10000, _ + 1) { i =>

        }
      }

      time("`loop` with 10000 iterations") {
        loop(1, 10000) { i =>

        }
      }

      val xs = List(5, 89, 30, 11, 20).tap("Initially").
               filter(_ < 30).tap("After filtering").
               map(_ * 2).tap("After mapping")

      println(xs)

      unless(xs.isEmpty) {
        println("xs is not empty.")
      }

      var i = 9
      repeat {
        println(i)
        i -= 1
      } until(i == 0)  
    }
  }
}