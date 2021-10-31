package image

import java.awt._
import java.awt.image._
import java.awt.geom.AffineTransform

import math._
import tool.Types._

object Operation {
  
  /** forall cell in mat: cell => cell.toInt.max(0).min(255) */
  def toColorMInt(mat: M): MInt = mat.map{_.map{_.toInt.max(0).min(255)}}
  
  /** generate simple image for wavelet test
   * @param isVisible Do visible result image?
   * default value isVisible = false
   **/
  def generateImageMat(isVisible: Boolean = false): M = {
    // B is amplitude
    // C is frequency
    val B: T = 5; val C: T = 10;
    val size = 100
    val picture: M = tool.Types.createWhiteMat(size, size)
    for (y <- 0 until size; x <- 0 until size)
      for (a <- 30 to 190 by 30)
      picture(y)(x) =  
        if (abs(y - a - B*sin(x/C)) < 6) 255-a
        else  min(picture(y)(x), 255)
    if (isVisible){
      val gRes: MInt = Operation.toColorMInt(picture)
      val newRGB = (gRes, gRes, gRes)
      val grayImg: BI = Operation.createImage(newRGB, BufferedImage.TYPE_3BYTE_BGR)
      Output.visible(grayImg, "outImg")
      Output.saveImage(grayImg, "out/generateImg.jpg", "jpg")
    }
    picture
  }
  def generateImageMat(amplitude: T, freq: T, w: Int, h: Int): BI = {
    val picture: M = tool.Types.createWhiteMat(w, h)
    for (y <- 0 until h; x <- 0 until w)
      for (a <- 30 to 190 by 30)
      picture(y)(x) =
        if (abs(y - a - amplitude * sin(x/freq)) < 6) 255-a
        else min(picture(y)(x), 255)
    val gRes: MInt = Operation.toColorMInt(picture)
    val newRGB = (gRes, gRes, gRes)
    Operation.createImage(newRGB, BufferedImage.TYPE_3BYTE_BGR)
  }
  
  /** full copy of image */
  def deepCopy(bi: BI): BI = {
    val cm: ColorModel = bi.getColorModel
    val raster: WritableRaster = bi.copyData(null)
    new BI(cm, raster, cm.isAlphaPremultiplied, null)
  }
  
  def simpleRotate(img: BI, theta: T): BI = {
    val w = img.getWidth; val h = img.getHeight
    val angle = theta*Pi/180
    val at = new AffineTransform()
    
    at.translate(w, h)
    at.rotate(angle)
    at.translate(-w/2, -h/2)
    
    affineTransform(img, at, w, h)
  }
  
  /** rotate image from theta degree (theta in (-180; 180])
   *  resImg bigger img: (w, h)==size(img) -> (R,R)==size(resImg)
   **/
  def rotate(img: BI, theta: T): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    val R = sqrt(sqr(img.getWidth)+sqr(img.getHeight)).toInt;
    resImg.getSubimage((curW-R)/2, (curH-R)/2, R, R)
  }
  
  /** rotate image from theta degree (theta in (-180; 180])
   *  new image as init image: (oldW,oldH) -> (R,R)==size(img) -> (oldW,oldH)
   **/
  def inverseRotate(img: BI, theta: T, oldW: Int, oldH: Int): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    resImg.getSubimage((curW-oldW)/2, (curH-oldH)/2, oldW, oldH)
  }
  
  /**
   * @param trans some affine transform
   * @param img used to obtain information about the image
   * @return new image on white background before transform
   */
  def affineTransform(img: BI, trans: AffineTransform, w: Int, h: Int): BI = {
    val resImg = new BI(2*w, 2*h, img.getType)
    val riGraphic = resImg.createGraphics()
    riGraphic.setBackground(Color.WHITE)
    riGraphic.clearRect(0, 0, 2*w, 2*h)
    resImg.getGraphics.asInstanceOf[ Graphics2D ].drawImage(img, trans, null)
    resImg
  }
  
   /**
    * rotate matrix 
    * @param mat -- matrix to rotate
    * @param theta -- rotate angle, from -180 to 180.
    * @param m -- usually, mat.length
    * @param n -- usually, mat(0).length
    * @see math definition as code
     * {{{
     * input: (y,x)
     * [m00 m01 m02] [x] = [m00x + m01y + m02]
     * [m10 m11 m12] [y] = [m10y + m11h + m12]
     * [0   0   0  ] [1] = [0    + 0    + 1  ]
     * 
     * 1. (y,x) => (y+h, x+w)
     * 2. (y,x) => [(c,-s); (s, c)]*(y,x)
     * 3. (y,x) => (y-h/2, x-w/2)
     * 
     * [1 0 w] [x] = [x + 0 + w]
     * [0 1 h] [y] = [0 + y + h]
     * [0 0 0] [1] = [0 + 0 + 1]
     * 
     * [c -s 0]   [1 0 -w/2]   [c -s  -cw/2+sh/2]
     * [s  c 0] * [0 1 -h/2] = [s  c  -sw/2-ch/2]
     * [0  0 1]		[0 0   1 ]	 [0  0       1    ]
     * 
     * [1 0 w]   [c -s  -cw/2+sh/2]   [c -s  -cw/2+sh/2+w]
     * [0 1 h] * [s  c  -sw/2-ch/2] = [s  c  -sw/2-ch/2+h]
     * [0 0 1] 	 [0  0       1    ]   [0  0        1     ]
     * 
     * [c -s  -cw/2+sh/2+w] [x]   [cx - sy -cw/2+sh/2+w]
     * [s  c  -sw/2-ch/2+h] [y] = [sx + cy -sw/2-ch/2+h]
     * [0  0       1      ] [1]   [        1           ]
     * }}}
     */
  def rotate(mat: M, theta: T, m: Int, n: Int): M = {
    val angle: T = Pi*theta/180
    val c = cos(angle); val s = sin(angle)
    
    val hM: T = 0.5*m; val hN: T = 0.5*n
    val addX = - c*hN + s*hM + n
    val addY = - s*hN - c*hM + m
    
    val res: M = createWhiteMat(2*m, 2*n)
    for (y <- 0 until m; x <- 0 until n){
      val X = (c*x - s*y + addX).round.toInt
      val Y = (s*x + c*y + addY).round.toInt
      res(Y)(X) = mat(y)(x)  
    }
    res
  }
  
  /** @see rotate(mat, theta, m, n) */
  def rotateFromMInt(mat: MInt, theta: T, m: Int, n: Int): M = 
    rotate(mat.map{_.map{_.toDouble}}, theta, m, n)
  
  def createImage(supMat: (MInt, MInt, MInt), imgType: Int): BI = {
    val r = supMat._1; val g = supMat._2; val b = supMat._3
    val h = r.length; val w = r(0).length;  
    val img = new BI(w, h, imgType)
    for(x <- 0 until w; y <- 0 until h){
      val c = b(y)(x)       +
             (g(y)(x) << 8) +
             (r(y)(x) << 16)
      img.setRGB(x, y, c)
    }
    img
  }
  
  def intMatrixToImage(mat: MInt): BI = {
    val newRGB = (mat, mat, mat)
    val grayImg = Operation.createImage(newRGB, BufferedImage.TYPE_3BYTE_BGR)
    grayImg
  }
  def matrixToImage(mat: M): BI = 
    intMatrixToImage(Operation.toColorMInt(mat))
  
  // without try for x
  def distance(img: BI, x: Int, y: Int, theta: T): Int = {
    def getCol(w: Int, h: Int): Int = 0xff & (img.getRGB(w, h)>>8)
    val height = img.getHeight; val width = img.getWidth;
    def isGood(w1: Int, h1: Int, w2: Int, h2: Int): Boolean = 
      abs(getCol(w1, h1) - getCol(w2, h2)) < 10
    val angle: T = Pi*((theta-90)/180)
    val tg: T = tan(angle)
    def getDY(dx: Int): Int = (dx*tg).floor.toInt.max(-y).min(height-y-1)
    var dx = 0
    while (x+dx < width && isGood(x, y, x+dx, y+getDY(dx))) dx += 1
    val d1 = dx
    dx = 0
    while (x+dx >= 0 && isGood(x, y, x+dx, y+getDY(dx))) dx -= 1
    d1-dx
  }
}