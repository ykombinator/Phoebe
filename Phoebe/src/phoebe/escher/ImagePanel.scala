package phoebe.escher

import javax.swing.JPanel
import java.awt.{Dimension, Graphics}

class ImagePanel(val image: Image, inset: Int = 0) extends JPanel {

  this.update_!

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    val panelDimensions = List(this.getWidth, this.getHeight).map(_ - 2 * inset)
    val imageDimensions = List(image.width, image.height)
    val ratio = (panelDimensions, imageDimensions).zipped.map(_.toDouble / _).min
    val newImageDimensions = imageDimensions.map(a => (a * ratio).toInt)
    val List(newImageWidth, newImageHeight) = newImageDimensions
    val List(x, y) = (panelDimensions, newImageDimensions).zipped.map((a, b) => (a - b) / 2)
    g.drawImage(image.base.getBufferedImage, x, y, newImageWidth, newImageHeight, this)
  }

  def update_! : Unit = {
    this.setPreferredSize(new Dimension(image.width, image.height))
    this.repaint()
  }
}