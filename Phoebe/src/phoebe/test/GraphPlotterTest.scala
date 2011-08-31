package phoebe.test

import phoebe.godel.GraphPlotter
import phoebe.escher.Color
import phoebe.Prelude._

object GraphPlotterTest {
  def main(args: Array[String]) {
    import GraphPlotter._
    import math._

    linePlot(
      x = 1.0 to 19.0 by 1.0,
      curves =
        (values(-3, 0, -4, 0, -5, 0, -4, 0, -3, 0, 3, 0, 4, 0, 5), "Stock Market", Color.Amethyst)
    )

    linePlot(
      x = 1.0 to 19.0 by 1.0,
      curves =
        (x => sin(x), "y = sin(x)", Color.Bisque)
    )

    gridPlot(
      x = 1.0 to 10.0 by 0.5,
      y = 1.0 to 10.0 by 0.5,
      curves =
        ((x, y) => x + cos(y), "z = x + cos(y)", Color.Green),
        ((x, y) => sin(x) + cos(y), "z = sin(x) + cos(y)", Color.Blue),
        ((x, y) => sin(x) + y, "z = sin(x) + y", Color.Red)
    )

    barPlot(
      x = 1.0 to 5.0 by 0.1,
      curves =
        (x => 4* x + 5, "4x + 5", Color.DarkGray)
    )

    scatterPlot(
      x = 1.0 to 5.0 by 0.1,
      curves =
        (x => 4 * x + 5, "4x + 5", Color.Cyan),
        (x => E ~^ x, "e ~^ x", Color.Teal)
    )

    staircasePlot(
      x = 1.0 to 5.0 by 0.1,
      curves =
        (x => E ~^ x, "e ~^ x", Color.Olive)
    )
  }
}