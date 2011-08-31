package phoebe.escher

import javax.swing.{WindowConstants, JFrame}

class SimpleImageViewer(val image: Image, val title: String = SimpleImageViewer.DefaultTitle) {
  private val frame = new JFrame(title)
  frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  private val imagePanel = new ImagePanel(image)
  frame.setContentPane(imagePanel)

  def show(): Unit = {
    if(!frame.isVisible) {
      frame.pack()
      frame.setVisible(true)
    }
  }

  def hide(): Unit = {
    if(frame.isVisible)
      frame.setVisible(false)
  }

  def update_! : Unit = {
    imagePanel.update_!
    frame.pack()
  }
}

object SimpleImageViewer {
  val DefaultTitle = "Phoebe - Simple Image Viewer"
}