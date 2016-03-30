package image

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

import tool.Types._

object Input {
  
 /**
  * @param name  name of file
  * @return image from file with name @name
  **/
  def getImage(name: String) = ImageIO.read(new File(name))
  
 /**
  * @param img some image
  * @return all pixels as array from raster data
  */
  def getPixels(img: BufferedImage): Array[Byte] = 
    img.getRaster.getDataBuffer( ).asInstanceOf[DataBufferByte].getData()
  
  def getColorsComponents(img: BI, colorID: Int) = {
    val n = img.getWidth; val m = img.getHeight
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) = (img.getRGB(x, y) >> (colorID-1)*8)&255 
    res
  }
  
 /**
  * fast method for read image
  * @see http://stackoverflow.com/questions/6524196/java-get-pixel-array-from-image
  * @param colorID 
  *  0 -- for black-white-gray image, or for 
  *  1 -- blue, 
  *  2 -- green, 
  *  3 -- red.
  * @return matrix with need color's components
  */
  def getColorsComponentsWithoutUsingGetRGB(img: BufferedImage, colorID: Int): MInt = {
    val w = img.getWidth; val h = img.getHeight
    val hasAlphaChannel = img.getAlphaRaster() != null
    def fun(x: Int): Int = 
      if (colorID == 0) x 
      else if (hasAlphaChannel) x<<2
           else x+x+x
    // color component's id from specific format
    val id = if (colorID == 0) 0
             else if (hasAlphaChannel) colorID
                  else colorID-1
                  
    val pixels: Array[Byte] = getPixels(img)
    val res: MInt = createMInt(h,w)
    for (i <- 0 until h) {
      val add = fun(i*w) + id
      for (j <- 0 until w) { 
        val x = pixels(fun(j) + add).toInt
        res(i)(j) = if (x < 0) x+256 else x
      }
    }
    res
  }
}