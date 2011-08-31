package phoebe.test

import phoebe.godel._

object ComplexTest {
  def main(args: Array[String]) {
    import phoebe.Prelude._
    val complexes = List(
      Complex.NaN,
      Complex.Zero,
      Complex.One,
      Complex.Infinity,
      Complex(3.0),
      Complex(-7.9),
      Complex(0.0, -3.4),
      Complex(0.0, 7.8),
      Complex(-2.3, 8.9),
      Complex(-2.4, -1.8),
      7.1 - 9.3.i,
      8.9 + 5.0.i
    )
    complexes.foreach(println)
  }
}