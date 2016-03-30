package image

import java.awt._
import java.io.File
import javax.imageio.ImageIO
import javax.swing._
import java.awt.image._
import tool.Types._

object Output {
  
  def saveImage(im: BufferedImage, fileName: String, format: String): Unit = 
    ImageIO.write(im, format, new File(fileName))

  /** visualization image through frame */
  def visible(image: BufferedImage, title: String) {
    val frame = new JFrame()
    val icon  = new ImageIcon(image)
    val label = new JLabel(icon)
    frame.getContentPane( ).add(label, BorderLayout.CENTER)
    frame.pack()
    frame.setName(title)
    frame.setTitle(title)
    frame.setVisible(true)
    frame.setDefaultCloseOperation( WindowConstants.DISPOSE_ON_CLOSE )
  }
  
    def visualisationAndSaveMat(mat: M, frameName: String, fileName: String) {
      val gRes = Operation.toColorMInt(mat)
      val newRGB = (gRes, gRes, gRes)
      val grayImg = Operation.createImage(newRGB, BufferedImage.TYPE_3BYTE_BGR)
      Output.visible(grayImg, frameName)
      Output.saveImage(grayImg, fileName, "jpg")
    }
}