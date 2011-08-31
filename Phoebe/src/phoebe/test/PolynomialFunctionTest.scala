package phoebe.test

import phoebe.godel.PolynomialFunction

object PolynomialFunctionTest {
  def main(args: Array[String]) {
    def testPrint(p: PolynomialFunction) {
      println(p + " >-> At x = 2: " + (p at 2) + " >-> Degree: " + p.degree) 
    }
    val func = PolynomialFunction(2, 9, 6, 2)
    testPrint(func)
    val d1 = func.derivative
    testPrint(d1)
    val d2 = d1.derivative
    testPrint(d2)
    val d3 = d2.derivative
    testPrint(d3)
    val d4 = d3.derivative
    testPrint(d4)
    val d5 = d4.derivative
    testPrint(d5)
    println(func.coefficients)
  }
}