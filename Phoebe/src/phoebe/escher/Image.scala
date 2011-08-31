package phoebe.escher

import marvin.image.MarvinImage
import marvin.io.MarvinImageIO

import phoebe.Prelude._

class Image private[escher](private[escher] var base: MarvinImage) extends Proxy {
  import Image._

  override def self = base

  override def toString = "Dimensions: %s, Format: %s" format (dimensions, formatName)

  // To be used for debugging
  def printPixels(rowIndices: Range, columnIndices: Range): Unit = {
    for(i <- rowIndices) {
      val row = for(j <- columnIndices) yield "#" + (this(i, j) & 0xffffff).toHexString
      println(row.mkString("[", ",", "]"))
    }
  }

  override def clone: Image = new Image(base.clone)

  def crop(x: Int, y: Int, width: Int, height: Int): Image = {
    this.clone.performing { im =>
      im.crop_!(x, y, width, height)
    }
  }

  def crop_!(x: Int, y: Int, width: Int, height: Int): Unit = {
    base = base.crop(x, y, width, height)
    base.update()
  }

  def clear: Image = {
    this.clone.performing { im =>
      im.clear_!
    }
  }

  def clear_! : Unit = {
    base.clearImage(0)
    base.update()
  }

  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Image = {
    this.clone.performing { im =>
      im.drawLine_!(x1, y1, x2, y2, color)
    }
  }

  def drawLine_!(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    base.drawLine(x1, y1, x2, y2, color.awtColor)
    base.update()
  }

  def drawRectangle(x: Int, y: Int, width: Int, height: Int, color: Color): Image = {
    this.clone.performing { im =>
      im.drawRectangle_!(x, y, width, height, color)
    }
  }

  def drawRectangle_!(x1: Int, y1: Int, x2: Int, y2: Int, color: Color): Unit = {
    base.drawRect(x1, y1, x2, y2, color.awtColor)
    base.update()
  }

  def fillRectangle(x: Int, y: Int, width: Int, height: Int, color: Color): Image = {
    this.clone.performing { im =>
      im.fillRectangle_!(x, y, width, height, color)
    }
  }

  def fillRectangle_!(x: Int, y: Int, width: Int, height: Int, color: Color): Unit = {
    base.fillRect(x, y, width, height, color.awtColor)
    base.update()
  }

  def resize(width: Int, height: Int): Image = {
    this.clone.performing { im =>
      im.resize_!(width, height)
    }
  }

  def resize_!(width: Int, height: Int): Unit = {
    base.resize(width, height)
  }

  def update(x: Int, y: Int, color: Int): Unit = {
    base.setIntColor(x, y, color)
    forceUpdatePixel_!(x, y, color)
  }

  def updated(x: Int, y: Int, color: Int): Image = {
    this.clone performing { im =>
      im(x, y) = color
    }
  }

  private def forceUpdatePixel_!(x: Int, y: Int, color: Int): Unit = {
    base.getBufferedImage.getWritableTile(-1, -1).setDataElements(x, y, base.getBufferedImage.getColorModel.getDataElements(color, null))
  }

  private def setColorArray_!(buffer: Array[Int]): Unit = {
    base.setIntColorArray(buffer)
    base.update()
  }

  def map_!(f: Int => Int): Unit = {
    val buffer = base.getIntColorArray
    for(i <- buffer.indices) {
      buffer(i) = f(buffer(i))
    }
    base.update()
  }

  def map(f: Int => Int): Image = {
    this.clone performing { im =>
      im.map_!(f)
    }
  }

  def zipWith(that: Image)(f: (Int, Int) => Int): Image = {
    assert(this.dimensions == that.dimensions)
    val List(abuffer, bbuffer) = List(this, that).map(_.base.getIntColorArray)
    val cbuffer = new Array[Int](abuffer.length)
    for(i <- abuffer.indices) {
      cbuffer(i) = f(abuffer(i), bbuffer(i))
    }
    Image.withBuffer(cbuffer, width, height)
  }

  def flip_! : Unit = {
    for {
      i <- 0 until height / 2
      j <- 0 until width
    } {
      val temp = this(j, i)
      this(j, i) = this(j, height - i - 1)
      this(j, height - i - 1) = temp
    }
  }

  def flip: Image = {
    this.clone performing { im =>
      im.flip_!
    }
  }

  def flop_! : Unit = {
    for {
      i <- 0 until width / 2
      j <- 0 until height
    } {
      val temp = this(i, j)
      this(i, j) = this(width - i - 1, j)
      this(width - i - 1, j) = temp
    }
  }

  def flop: Image = {
    this.clone performing { im =>
      im.flop_!
    }
  }

  def sliding(mask: Vector[Vector[Double]])(f: Seq[Int] => Int): Image = {
    val s = mask.size / 2
    Image(width, height) performing { out =>
      for {
        i <- s to width - s - 1
        j <- s to height - s -1
      } {
        out(i, j) = {
          val Seq(rs, gs, bs) = (for {
            k <- -s to s
            l <- -s to s
          } yield {
            val Color(r, g, b) = this(i + k, j + l)
            Seq(r, g, b).map(x => (x * mask(s + k)(s + l)).toInt)
          }).transpose
          Color(
            Image.boundColor(f(rs)),
            Image.boundColor(f(gs)),
            Image.boundColor(f(bs))
          )
        }
      }
    }
  }

  def edges(mask: Vector[Vector[Double]]): Image = {
    this.sliding(mask)(_.sum)
  }

  def edgesWithSobel: Image = edges(
    %(
      %(-2, -2, 0),
      %(-2, 0, 2),
      %(0, 2, 2)
    )
  )

  def edgesWithPrewitt: Image = edges(
    %(
      %(-2, -1, 0),
      %(-1, 0, 1),
      %(0, 1, 2)
    )
  )

  def medianFilter: Image = {
    this.sliding(Vector.fill(3)(Vector.fill(3)(1)))(median)
  }

  def soften: Image = {
    this.sliding(Vector.fill(3)(Vector.fill(3)(1)))(v => v.sum / 9)
  }

  // A new invention. Old trick doesn't work.
  def sharpen: Image = {
    this.sliding(
      %(
        %(0, -1, 0),
        %(-1, 5, -1),
        %(0, -1, 0)
      )
    )(_.sum)
  }

   def blur: Image = {
    this.sliding(
      %(
        %(0, 1.0/8, 0),
        %(1.0/8, 1.0/2, 1.0/8),
        %(0, 1.0/8, 0)
      )
    )(_.sum)
  }

  def bitPlaneAt(position: Int): Image = this.toGrayscale map { p =>
    val x = (p bitAt position) * 255
    Color(x, x, x)
  }


  def emboss: Image = {
    this.sliding(
      %(
        %(-1, -1, -1),
        %( 0,  0,  0),
        %( 1,  1,  1)
      )
    )(_.sum)
  }

  def invert: Image = this map { case Color(r, g, b) =>
    Color(255 - r, 255 - g, 255 - b)
  }


  def threshold(t: Int): Image = this.toGrayscale map { p =>
    if(p > Color(t, t, t)) Color.White else Color.Black
  }

  def grayLevelSlice(min: Int, max: Int, withBackground: Boolean = false): Image = this.toGrayscale map { p =>
    if(p > Color(min, min, min) && p <= Color(max, max, max)) Color.White else {
      if(withBackground) (p: Color) else Color.Black
    }
  }

  def foreach(f: Int => Unit): Unit = {
    base.getIntColorArray.foreach(f)
  }

  def iterator: Iterator[Iterator[Int]] = {
    val image = this
    new Iterator[Iterator[Int]] {
      private var rowIndex = 0

      def hasNext: Boolean = rowIndex < height

      def next: Iterator[Int] = {
        val rowIndexFixated = rowIndex
        val rowIterator = new Iterator[Int] {
          private var columnIndex = 0

          def next: Int = {
            val p = image(rowIndexFixated, columnIndex)
            columnIndex += 1
            p
          }

          def hasNext: Boolean = columnIndex < width
        }
        rowIndex += 1
        rowIterator
      }
    }
  }


  def toGrayscale: Image = this map { case Color(r, g, b) =>
    val x = (r * 0.3 + g * 0.59 + b * 0.11).toInt
    Color(x, x, x)
  }

  def display(title: String = SimpleImageViewer.DefaultTitle): Unit = {
    val viewer = new SimpleImageViewer(this, title)
    viewer.show()
  }

  def formatName: String = base.getFormatName

  def height: Int = base.getHeight

  def width: Int = base.getWidth

  def dimensions: (Int, Int) = (width, height)

  def apply(x: Int, y: Int): Int = base.getIntColor(x, y)

  def redAt(x: Int, y: Int): Int = base.getIntComponent0(x, y)

  def greenAt(x: Int, y: Int): Int = base.getIntComponent1(x, y)

  def blueAt(x: Int, y: Int): Int = base.getIntComponent2(x, y)

  def writeTo(filePath: String): Unit = {
    MarvinImageIO.saveImage(this.base, filePath)
  }

  val ->> = writeTo _
}

object Image {
  val DefaultImageViewerTitle = "Image Viewer"

  def apply(width: Int, height: Int): Image = {
    new Image(new MarvinImage(width, height))
  }

  private def withBuffer(buffer: Array[Int], width: Int, height: Int): Image = {
    Image(width, height) performing { im =>
      im.setColorArray_!(buffer)
    }
  }

  def boundColor(color: Int): Int = {
    if(color < 0)
      0
    else if(color > 255)
      255
    else
      color
  }
  
  def from(filePath: String): Image = {
    new Image(MarvinImageIO.loadImage(filePath))
  }

  val <<- = from _

  private def median[A : Ordering](xs: Seq[A]): A = xs.sorted.apply(xs.size / 2)
}

