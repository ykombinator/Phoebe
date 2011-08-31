package phoebe.godel

import org.apache.commons.math.analysis.integration.TrapezoidIntegrator
import org.apache.commons.math.analysis.UnivariateRealFunction

object Integrator {
  def integrate(f: Double => Double) = new Integration(f)

  class Integration private[Integrator](f: Double => Double) {
    def within(min: Double, max: Double): Double = {
      val func = new UnivariateRealFunction {
        override def value(x: Double): Double = f(x)
      }
      (new TrapezoidIntegrator).integrate(func, min, max)
    }
  }
}