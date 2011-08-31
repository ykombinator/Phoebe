package phoebe.test

import phoebe.escher._
import phoebe.Prelude._

object GridImageViewerTest {
  def main(args: Array[String]) {
    val x = Image from "E:/metallica.jpg"
    val y = Image from "E:/porcupinetree.jpg"
    val viewer = new GridImageViewer(
      %(
        %((x, "a"), (y, "b"), (x, "c")),
        %((x, "p"), (y, "q"), (y, "r")),
        %((x, "y"), (x, "O"))
      )
    )
    viewer.show()
    /*x.crop_!(0, 0, 100, 100)
    y.crop_!(0, 0, 100, 100)*/
  }
}