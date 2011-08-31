package phoebe.escher

import javax.swing.{JLabel, JFrame, WindowConstants}
import java.awt._

//
// A big image title may mess up the image grid.
//
class GridImageViewer(val images: Vector[Vector[(Image, String)]], val title: String = GridImageViewer.DefaultTitle) {

  private val nRows = images.length
  private val nColumns = images.map(_.length).max

  private val frame = new JFrame(title)
  frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  frame.setMinimumSize(new Dimension(200, 200))

  private val gridBagLayout = new GridBagLayout
  frame.setLayout(gridBagLayout)

  for {
    (row, r) <- images.zipWithIndex
    ((image, label), c) <- row.zipWithIndex
  } {
    val constraints = new GridBagConstraints

    // draw the image
    constraints.gridx = c
    constraints.gridy = 2 * r
    constraints.gridwidth = 1
    constraints.gridheight = 1
    constraints.weightx = 1000
    constraints.weighty = 500
    constraints.fill = GridBagConstraints.BOTH
    constraints.anchor = GridBagConstraints.CENTER
    val inset = 3
    constraints.insets = new Insets(inset, inset, inset, inset)
    val panel = new ImagePanel(image, inset)
    gridBagLayout.setConstraints(panel, constraints)
    frame.add(panel)

    // draw the label
    constraints.gridy = 2 * r + 1
    constraints.weighty = 0
    constraints.fill = GridBagConstraints.NONE
    val jLabel = new JLabel(label)
    gridBagLayout.setConstraints(jLabel, constraints)
    frame.add(jLabel)
  }

  def show(): Unit = {
    if(!frame.isVisible) {
      frame.setExtendedState(frame.getExtendedState | Frame.MAXIMIZED_BOTH)
      frame.setVisible(true)
    }
  }

  def hide(): Unit = {
    if(frame.isVisible)
      frame.setVisible(false)
  }

  def update_! : Unit = {
    frame.repaint()
  }
}

object GridImageViewer {
  val DefaultTitle = "Phoebe - Grid Image Viewer"
}