package phoebe.godel

class Ratio(n: Int, d: Int) extends Ordered[Ratio] {

  require {
    d != 0
  }

  lazy val (numerator, denominator) = {
    def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
    val g = gcd(n , d).abs
    if(d < 0)
      (-n / g, -d / g)
    else
      (n / g, d / g)
  }

  def +(that: Ratio) = new Ratio(
    this.numerator * that.denominator + that.numerator * this.denominator,
    this.denominator * that.denominator
  )

  def -(that: Ratio) = this + -that

  def *(that: Ratio) = new Ratio(
    this.numerator * that.numerator,
    this.denominator * that.denominator
  )

  def /(that: Ratio) = this * that.reciprocal

  def reciprocal = new Ratio(denominator, numerator)

  def unary_- = new Ratio(-numerator, denominator)

  override def compare(that: Ratio): Int = {
    this.numerator * that.denominator - that.numerator * this.denominator
  }

  override def toString = numerator + " \\ " + denominator
}

object Ratio {
  implicit def doubleToRatio(d: Double) = new Ratio((d * 10e5).toInt, (10e5).toInt)
  implicit def ratioToDouble(r: Ratio) = r.numerator.toDouble / r.denominator
}

object \ {
  def unapply[D <% Double](d: D): Option[(Int, Int)] = {
    val r: Ratio = d.toDouble
    Some(r.numerator, r.denominator)
  }
}