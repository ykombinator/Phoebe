package phoebe.test

import phoebe.godel.Matrix

object MatrixTest {
  def main(args: Array[String]) {
    val mat = Matrix.tabulate(4, 3)(_ + _)
    val nat = Matrix.tabulate(3, 2)(_ + _)
    println(mat)
    println(nat)
    println(mat * nat)
  }
}