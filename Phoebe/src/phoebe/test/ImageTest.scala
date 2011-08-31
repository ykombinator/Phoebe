package phoebe.test

import phoebe.Prelude._
import phoebe.escher.{GridImageViewer, Color, Image}
import collection.mutable.{ArrayBuffer, Buffer}

object ImageTest {
  def testBitPlane(image: Image) {
    new GridImageViewer(
      %(
        %((image, "i"), (image bitPlaneAt 0, "0"), (image bitPlaneAt 1, "1")),
        %((image bitPlaneAt 2, "2"), (image bitPlaneAt 3, "3"), (image bitPlaneAt 4, "4")),
        %((image bitPlaneAt 5, "5"), (image bitPlaneAt 6, "6"), (image bitPlaneAt 7, "7"))
      ),
    "Planes").show()
  }


  def testThreshold(image: Image) {
    image.threshold(128).display("Threshold")
  }

  def testGrayscale(image: Image) {
    image.toGrayscale.display("Grayscale")
  }

  def testGrayLevelSlice(image: Image) {
    val i = image.grayLevelSlice(150, 250, withBackground = true)
    i.display("Sliced")
    i ->> "E:/PhoebeSliced.jpg"
  }
  def testZipWith(image: Image) {
    (image.map(_ & Color.Red) zipWith image.map(_ & Color.Blue))(_ + _).display("Zippa!")
  }

  def testFlip(image: Image)  {
    image.flip.display("flipPig")
  }

  def testSharpen(image: Image): Unit = {
    val out = image.sharpen
    new GridImageViewer(
      %(
        %((image, "ori"), (out, "out"))
      )
    ).show()
  }

  def main(args: Array[String]): Unit = {
    val saturn = Image from "pics/Saturn.jpg"
    val ocean = Image from "pics/Ocean.jpg"
    (saturn zipWith ocean)(_ | _) ->> "pics/SaturnInOcean.jpg"
  }
}