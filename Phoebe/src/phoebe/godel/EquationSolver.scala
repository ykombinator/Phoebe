package phoebe.godel

import phoebe.Prelude._

object EquationSolver {
  def solveQuadratic(a: Double, b: Double, c: Double): (Complex, Complex) = {
    val d = b ~^ 2 - 4 * a * c
    if(d == 0) {
      val root = Complex(-b / (2 * a))
      (root, root)
    } else if(d > 0) {
      val root1 = Complex((-b + math.sqrt(d)) / (2 * a))
      val root2 = Complex((-b - math.sqrt(d)) / (2 * a))
      (root1, root2)
    } else {
      val root = Complex(-b / (2 * a) , math.sqrt(d.abs) / (2 * a))
      (root, root.conjugate)
    }
  }
}