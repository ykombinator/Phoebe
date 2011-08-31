package phoebe.test

import phoebe.Prelude._
import collection.mutable.ArrayBuffer

object SlidingTest {
  type ArrayBuffer2D = ArrayBuffer[ArrayBuffer[Int]]

  def sliding(image: ArrayBuffer2D, mask: ArrayBuffer2D)(f: IndexedSeq[Int] => Int): ArrayBuffer2D = {
    val s = mask.size / 2
    ArrayBuffer.tabulate(8, 8)((_, _) => 0) performing { out =>
      for {
        i <- s to 8 - s - 1
        j <- s to 8 - s -1
      } {
        out(i)(j) = f(
          for {
            k <- -s to s
            l <- -s to s
          } yield image(i + k)(j + l) * mask(s + k)(s + l)
        )
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val image = ArrayBuffer(
      ArrayBuffer.fill(8)(10),
      ArrayBuffer.fill(8)(10),
      ArrayBuffer.fill(8)(10),
      ArrayBuffer.fill(8)(10),
      ArrayBuffer.fill(8)(50),
      ArrayBuffer.fill(8)(50),
      ArrayBuffer.fill(8)(50),
      ArrayBuffer.fill(8)(50)
    )

    // Soften
    val mask = ArrayBuffer.fill(3)(ArrayBuffer.fill(3)(1))
    sliding(image, mask)(_.sum / 9) foreach (x => println(x.mkString("["," ", "]")))
  }
}