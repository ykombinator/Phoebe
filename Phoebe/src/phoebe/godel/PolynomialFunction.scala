package phoebe.godel

import org.apache.commons.math.analysis.polynomials.{
  PolynomialFunction => ApachePolynomialFunction,
  PolynomialsUtils => ApachePolynomialsUtils
}

class PolynomialFunction private(private val apf: ApachePolynomialFunction) extends Proxy {
  def this(coefficients: Double*) {
    this(new ApachePolynomialFunction(coefficients.toArray))
  }

  override def self = this.apf

  def +(that: PolynomialFunction) = new PolynomialFunction(this.apf add that.apf)

  def -(that: PolynomialFunction) = new PolynomialFunction(this.apf subtract that.apf)

  def *(that: PolynomialFunction) = new PolynomialFunction(this.apf multiply that.apf)

  def unary_- = new PolynomialFunction(this.apf.negate)

  def degree = this.apf.degree

  lazy val coefficients = Vector(this.apf.getCoefficients: _*)

  def derivative = new PolynomialFunction(this.apf.polynomialDerivative)

  def apply(x: Double): Double = this.apf.value(x)

  val at = this.apply _
}

object PolynomialFunction {
  def apply(coefficients: Double*) = new PolynomialFunction(coefficients: _*)
  def chebyshev(degree: Int) = new PolynomialFunction(ApachePolynomialsUtils.createChebyshevPolynomial(degree))
  def hermite(degree: Int) = new PolynomialFunction(ApachePolynomialsUtils.createHermitePolynomial(degree))
  def laguerre(degree: Int) = new PolynomialFunction(ApachePolynomialsUtils.createLaguerrePolynomial(degree))
  def legendre(degree: Int) = new PolynomialFunction(ApachePolynomialsUtils.createLegendrePolynomial(degree))
}