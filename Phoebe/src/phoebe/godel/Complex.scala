package phoebe.godel

import collection.mutable
import phoebe.Prelude._

case class Complex(real: Double, imaginary: Double = 0.0) {

  def isNaN = real.isNaN || imaginary.isNaN

  def isInfinite = !isNaN && (real.isInfinite || imaginary.isInfinite)

  def abs = math.sqrt(real ~^ 2 + imaginary ~^ 2)

  def +(that: Complex) = Complex(real + that.real, imaginary + that.imaginary)

  def -(that: Complex) = Complex(real - that.real, imaginary - that.imaginary) 

  def unary_- = Complex(-real, -imaginary)

  def *(that: Complex) = Complex(
    real * that.real - imaginary * that.imaginary,
    real * that.imaginary + imaginary * that.real
  )

  def /(that: Complex) = {
    val denom = that.imaginary ~^ 2 + that.real ~^ 2
    Complex(
      ((real * that.real) + (imaginary * that.imaginary)) / denom,
      ((imaginary * that.real) - (real * that.imaginary)) / denom
    )
  }

  def reciprocal = Complex.One / this

  def conjugate = Complex(real, -imaginary)

  def log = Complex(math.log(this.abs), math.atan2(imaginary, real))

  def ~^(that: Complex) = (this.log * that).exp

  def acos = this + (this.sqrt1z * Complex.I).log * -Complex.I

  def asin = this.sqrt1z + (this * Complex.I).log * -Complex.I

  def atan = (this + Complex.I) / (Complex.I - this).log * Complex.I / Complex(2.0)

  def cos = Complex(
    math.cos(real) * math.cosh(imaginary),
    -math.sin(real) * math.sinh(imaginary)
  )

  def cosh = Complex(
    math.cosh(real) * math.cos(imaginary),
    math.sinh(real) * math.sin(imaginary)
  )

  def exp = {
    val expOfReal = math.exp(real)
    Complex(
      expOfReal * math.cos(imaginary),
      expOfReal * math.sin(imaginary)
    )
  }

  def sin = Complex(
    math.sin(real) * math.cosh(imaginary),
    math.cos(real) * math.sinh(imaginary)
  )

  def sinh = Complex(
    math.sinh(real) * math.cos(imaginary),
    math.cosh(real) * math.sin(imaginary)
  )

  def sqrt = this ~^ Complex(0.5)

  def sqrt1z = (Complex.One - this ~^ Complex(2.0)).sqrt

  def tan = {
    val real2 = 2.0 * real
    val imaginary2 = 2.0 * imaginary
    val d = math.cos(real2) + math.cosh(imaginary2)
    Complex(math.sin(real2) / d, math.sinh(imaginary2) / d)
  }

  def tanh = {
    val real2 = 2.0 * real
    val imaginary2 = 2.0 * imaginary
    val d = math.cosh(real2) + math.cos(imaginary2)
    Complex(math.sinh(real2) / d, math.sin(imaginary2) / d)
  }

  def argument = math.atan2(imaginary, real)

  def nthRoots(n: Int): List[Complex] = {
    if(n <= 0)
      error("Non-positive n.")
    else if(this.isNaN)
      List(Complex.NaN)
    else if(this.isInfinite)
      List(Complex.Infinity)
    else {
      val nthRootOfAbs = this.abs ~^ (1.0 / n)
      val nthPhi = argument / n
      val slice = 2 * math.Pi / n
      var innerPart = nthPhi
      val result = new mutable.ArrayBuffer[Complex]
      n times {
        val realPart = nthRootOfAbs * math.cos(innerPart)
        val imaginaryPart = nthRootOfAbs * math.sin(innerPart)
        result += Complex(realPart, imaginaryPart)
        innerPart += slice
      }
      result.toList
    }
  }

  override def toString = {
    if(this.isNaN)
      "NaN"
    else if(this.isInfinite)
      "Infinity"
    else if(real == 0.0 && imaginary == 0.0)
      "0.0"
    else if(real == 0.0) {
      imaginary + "i"
    } else {
      if(imaginary == 0.0)
        real.toString
      else
        real + (if(imaginary > 0.0) " + " else " - ") + imaginary.abs + "i"
    }
  }
}

object Complex {
  implicit def doubleToComplex(d: Double) = Complex(d)

  val I = Complex(0.0, 1.0)
  val NaN = Complex(Double.NaN, Double.NaN)
  val Infinity = Complex(Double.PositiveInfinity, Double.PositiveInfinity)
  val One = Complex(1.0)
  val Zero = Complex(0.0)

  def abs(c: Matrix[Complex]): Matrix[Double] = c.map(_.abs)

  implicit object ComplexNumeric extends Numeric[Complex] {
    def plus(a: Complex, b: Complex): Complex = a + b
    def minus(a: Complex, b: Complex): Complex = a - b
    def times(a: Complex, b: Complex): Complex = a * b
    def negate(a: Complex): Complex = -a
    def fromInt(i: Int) = Complex(i)

    def toInt(a: Complex) = error("This conversion doesn't make any damn sense.")
    def toLong(a: Complex) = error("Are you kidding me?")
    def toFloat(a: Complex) = error("How many joints? xD")
    def toDouble(a: Complex) = error("Do you even remotely know maths?!")

    override def zero = Complex.Zero
    override def one = Complex.One

    def compare(a: Complex, b: Complex): Int = {
      if(a.real == b.real)
        a.imaginary.compare(b.imaginary)
      else
        a.real.compare(b.real)
    }
  }
}
