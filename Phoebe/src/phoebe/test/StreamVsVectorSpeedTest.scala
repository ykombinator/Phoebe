package phoebe.test

import phoebe.Prelude._

object StreamVsVectorSpeedTest {
  def main(args: Array[String]) {
    time("Stream") {
      val stream = Stream.iterate(10, 10000)(_ * 2)
      println(stream.map(_ * 2).filter(_ % 3 != 0).foldLeft(0)(_ + _ / 2))
    }

    time("Vector") {
      val vector = Vector.iterate(10, 10000)(_ * 2)
      println(vector.map(_ * 2).filter(_ % 3 != 0).foldLeft(0)(_ + _ / 2))
    }

    time("Stream Forced") {
      val stream = Stream.iterate(10, 10000)(_ * 2).force
      println(stream.map(_ * 2).filter(_ % 3 != 0).foldLeft(0)(_ + _ / 2))
    }

    time("List") {
      val list = List.iterate(10, 10000)(_ * 2)
      println(list.map(_ * 2).filter(_ % 3 != 0).foldLeft(0)(_ + _ / 2))
    }

    /*
    Results suggest that the Vector is the fastest on the warmed up JVM, closely followed by Stream-forced and List.
     */
  }
}