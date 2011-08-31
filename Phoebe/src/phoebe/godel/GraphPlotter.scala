package phoebe.godel

import collection.immutable.NumericRange
import math._
import java.awt.{Color => AwtColor}

import org.math.plot.{Plot3DPanel, Plot2DPanel, PlotPanel}

import phoebe.Prelude._
import phoebe.escher.Color
import javax.swing.{WindowConstants, JFrame}

object GraphPlotter {

  val WindowTitle = "Graph Plotter"

  type Graph2DFunction = Double => Double
  type Graph3DFunction = (Double, Double) => Double
  type Legend = String

  type Curve2D = (Graph2DFunction, Legend, Color)
  type Curve3D = (Graph3DFunction, Legend, Color)

  private def display(plot: PlotPanel) {
    val frame = new JFrame(WindowTitle)
    locally {
      import frame._
      setSize(600, 600)
      setContentPane(plot)
      setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      setVisible(true)
    }
  }

  private def computePointsAndPlot(x: NumericRange[Double], curves: Seq[Curve2D])
                                  (plotIt: (Plot2DPanel, Legend, AwtColor, Array[Array[Double]]) => Unit) {
    val plot = new Plot2DPanel("SOUTH")
    for(curve <- curves) {
      val (f, legend, color) = curve
      val z = new Array[Array[Double]](x.length, 2)
      for(i <- x.indices) {
        z(i)(0) = i
        z(i)(1) = f(x(i))
      }
      plotIt(plot, legend, color.awtColor, z)
    }
    display(plot)
  }
 
  def linePlot(x: NumericRange[Double], curves: Curve2D*) {
    computePointsAndPlot(x, curves) { (p, l, c, z) =>
      p.addLinePlot(l, c, z)
    }
  }

  def scatterPlot(x: NumericRange[Double], curves: Curve2D*) {
    computePointsAndPlot(x, curves) { (p, l, c, z) =>
      p.addScatterPlot(l, c, z)
    }
  }

  def staircasePlot(x: NumericRange[Double], curves: Curve2D*) {
    computePointsAndPlot(x, curves) { (p, l, c, z) =>
      p.addStaircasePlot(l, c, z)
    }
  }

  def barPlot(x: NumericRange[Double], curves: Curve2D*) {
    computePointsAndPlot(x, curves) { (p, l, c, z) =>
      p.addBarPlot(l, c, z)
    }
  }
  
  def gridPlot(x: NumericRange[Double], y: NumericRange[Double], curves: Curve3D*) {
    val plot = new Plot3DPanel("SOUTH")
    val List(xArray, yArray) = List(x, y).map(_.toArray) 
    for(curve <- curves) {
      val (f, legend, color) = curve
      val z = new Array[Array[Double]](y.length, x.length)
      for(i <- x.indices; j <- y.indices)
        z(j)(i) = f(x(i), y(j))
      plot.addGridPlot(legend, color.awtColor, xArray, yArray, z)
    }
    display(plot)
  }

  def values(f: Double*): Double => Double = asGraphingFunction(f.toIndexedSeq)

  def asGraphingFunction[A, B](f: A =>? B)(implicit numA: Numeric[A], numB: Numeric[B]): Graph2DFunction = {
    (x: Double) => f.lift(numA.fromInt(x.toInt)).map(numB.toDouble).getOrElse(0.0)
  }
}