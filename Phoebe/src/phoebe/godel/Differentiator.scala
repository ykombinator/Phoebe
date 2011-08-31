package phoebe.godel

import phoebe.escher.Color

object Differentiator {
  private val EpsilonSquareroot = 1.4832396974191326E-8

  def ddx(f: Double => Double) = new FirstOrderDerivative(f)

  class FirstOrderDerivative private[Differentiator](f: Double => Double) {
    def at(x: Double): Double = {
      val h = if(x!= 0) (EpsilonSquareroot * x) else EpsilonSquareroot
      (f(x + h) - f(x)) / h
    }
  }

  def d2dx2(f: Double => Double) = new SecondOrderDerivative(f)

  class SecondOrderDerivative private[Differentiator](f: Double => Double) {
    def at(x: Double): Double = {
      val h = EpsilonSquareroot * x * x
      (f(x + h) - 2 * f(x) + f(x - h)) / (h * h)
    }
  }
}