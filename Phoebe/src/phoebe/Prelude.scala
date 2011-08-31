package phoebe

import phoebe.godel._
import escher.Color

object Prelude {

  /****************************** Console Input Functions ******************************************/

  def readLine(prompt: String): String = {
    print(prompt)
    Console.readLine()
  }

  def readInt(prompt: String): Int = readLine(prompt).toInt

  def readDouble(prompt: String): Double = readLine(prompt).toDouble

  /****************************** Control Construsts ***********************************************/

  def time(description: String)(f: => Unit): Long = {
    val start = System.nanoTime
    f
    val end = System.nanoTime
    val timeTaken = end - start
    println("%s: %,d nanoseconds." format (description, timeTaken))
    timeTaken
  }

  def cfor(begin: Int, condition: Int => Boolean, change: Int => Int)(f: Int => Unit) {
    var i = begin
    while(condition(i)) {
      f(i)
      i = change(i)
    }
  }

  def loop(begin: Int, end: Int)(f: Int => Unit) {
    var i = begin
    while(i <= end) {
      f(i)
      i += 1
    }
  }

  def unless(condition: => Boolean)(f: => Unit) {
    if(!condition)
      f
  }

  def repeat(f: => Unit) = new RepeatUntilBlock(f)

  class RepeatUntilBlock private[Prelude](f: => Unit) {
    def until(condition: => Boolean) {
      do {
        f
      } while(!condition)
    }
  }

  def skip[A](f: => A): Unit = {
    /* Do nothing. (Not an easy job, pal.) */
  }

  def condition[A](cond: => Boolean, a: => A, b: => A): A = if(cond) a else b

  implicit def anyToTappable[A : Manifest](a: A) = new Tappable(a)

  class Tappable[A : Manifest] private[Prelude](a: A) {
    def tap: A = tap("<no-message>")

    def tap(message: String): A = {
      println(message + ": " + a + "  [ Typed: " + manifest[A] + " ]")
      a
    }
  }
  
  /****************************** Utility Functions ************************************/

  def sigma(f: Int => Double) = new Sigma(f)

  class Sigma private[Prelude](f: Int => Double) {
    def within(start: Int, end: Int): Double = (start to end).map(f).sum
  }

  def arithmeticProgression[A](initialTerm: A, commonDifference: A)(implicit numericEv: Numeric[A]): Stream[A] = {
    import numericEv._
    Stream.iterate(initialTerm)(_ + commonDifference)
  }

  def geometricProgression[A](initialTerm: A, commonRatio: A)(implicit numericEv: Numeric[A]): Stream[A] = {
    import numericEv._
    Stream.iterate(initialTerm)(_ * commonRatio)
  }

  // Y combinator
  object Y {
    def apply[A, B](f: (A => B) => (A => B)): (A => B) = (a: A) => f(Y(f))(a)
  }

  /****************************** Pimps ***********************************************/

  class EnrichedInt private[Prelude](a: Int) {
    def times(block: => Unit) {
      times(i => block)
    }

    def times(block: Int => Unit) {
      var i = 0
      while(i < a) {
        block(i)
        i += 1
      }
    }

    def bitAt(position: Int) = (a >> position) & 1
    def \(b: Int) = new Ratio(a, b)
    def big = BigInt(a)
  }

  class EnrichedDouble private[Prelude](a: Double) {
    def +(c: Complex) = Complex(a) + c
    def -(c: Complex) = Complex(a) - c
  }

  class EnrichedDoubleLike[A <% Double] private[Prelude](a: A) {
    def ~^(power: A): Double = math.pow(a, power)
    def i = Complex(0.0, a)
  }

  class EnrichedIterable[+A <% Ordered[A]] private[Prelude](xs: Iterable[A]) {
    def sequenceTest(predicate: (A, A) => Boolean): Boolean = {
      (xs, xs.tail).zipped.forall(predicate)
    }

    def hasAllElementsEqual = sequenceTest(_ == _)
    def isAscending = sequenceTest(_ <= _)
    def isDescending = sequenceTest(_ >= _)
  }

  class EnrichedIntegral[A] private[Prelude](a: A)(implicit integralEv: Integral[A]) {
    import integralEv._

    def isOdd = a % fromInt(2) != zero
    def isEven = a % fromInt(2) == zero
    def |?(b: A) = rem(a, b) == 0

    def ! : A = {
      var f, i = one
      while(i <= a) {
        f *= i
        i += one
      }
      f
    }
  }

  class EnrichedNumeric[A] private[Prelude](a: A)(implicit numericEv: Numeric[A]) {
    import numericEv._
    def ~(b: A) = (a - b).abs
    def isPositive = a > zero
    def isNegative = a < zero
    def isNonNegative = a >= zero
    def isNonPositive = a <= zero
  }

  class EnrichedAny[A] private[Prelude](a: A) {
    def performing(f: A => Unit): A = {
      f(a)
      a
    }
  }

  implicit def enrichInt(n: Int) = new EnrichedInt(n)
  implicit def enrichDouble(n: Double) = new EnrichedDouble(n)
  implicit def enrichDoubleLike[A <% Double](n: A) = new EnrichedDoubleLike(n)
  implicit def enrichIntegral[I : Integral](n: I) = new EnrichedIntegral(n)
  implicit def enrichNumeric[N : Numeric](n: N) = new EnrichedNumeric(n)
  implicit def enrichIterable[A <% Ordered[A]](xs: Iterable[A]) = new EnrichedIterable(xs)
  implicit def enrichAny[A](a: A) = new EnrichedAny(a)
  implicit def colorToInt(c: Color): Int = c.rgb
  implicit def intToColor(rgb: Int): Color = Color(rgb)

  /****************************** Type aliases ***********************************************/

  type %[+A] = Vector[A]
  val % = Vector

  type =>?[-A, +B] = PartialFunction[A, B]
}