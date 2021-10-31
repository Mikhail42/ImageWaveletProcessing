package image

import tool._
import tool.Types._
import math._

object ImageWaveletInterface {
  val black = 200
}
import ImageWaveletInterface._

/** object for test, e.g., some wavelets transform */
class ImageWaveletInterface(img: BI) {
  val m = img.getHeight; val n = img.getWidth
  
  def wavelet(transf: (M, T) => M, a: T, showDirectly: Boolean, isWhiteBackground: Boolean): ColectionBI = {
    val resList = new ColectionBI()
    import Operation._
    val globRes: M = 
      if (isWhiteBackground) Types.createWhiteMat(m, n)
      else Types.createM(m, n)
    
    val directly: MInt = Types.createMInt(m, n)
    
    // theta is image rotation angle
    (0 until 180 by 30).foreach(theta => {
      val rotImg: BI = rotate(img, theta)
      val rotMat: MInt = Input.getColorsComponents(rotImg, 2)
      
      val resTrans: M = transf(rotMat.map{_.map{255 - _.toDouble}}, a)
      val resTransAbs: M = resTrans.map{_.map{_.abs}}
      val transImg: BI = matrixToImage(resTransAbs)      
      
      val resImg: BI = inverseRotate(transImg, -theta, n, m)
      
      val resMat: MInt = Input.getColorsComponents(resImg, 2)
      
      if (!showDirectly) {
        if (isWhiteBackground) {
          for (y <- 0 until m; x <- 0 until n) 
            globRes(y)(x) = min(globRes(y)(x), 255-resMat(y)(x))
        }
        else {
          for (y <- 0 until m; x <- 0 until n) 
            globRes(y)(x) = max(globRes(y)(x), resMat(y)(x))
        }
      } else 
        if (isWhiteBackground) {
          for (y <- 0 until m; x <- 0 until n) 
            if (globRes(y)(x) > 255-resMat(y)(x)) {
              globRes(y)(x) = 255-resMat(y)(x)
              directly(y)(x) = theta
            }
        } else {
          for (y <- 0 until m; x <- 0 until n)
            if (globRes(y)(x) < resMat(y)(x)) {
              globRes(y)(x) = resMat(y)(x)
              directly(y)(x) = theta
            }
        }
    })
    
    if (showDirectly)
      resList.+=(intMatrixToImage(directly))
    
    val resMatImg = globRes.map{_.map{255 - _}}
    resList.+=( matrixToImage(resMatImg) )
    
    val mediateMat = createMBool(m, n)
    for (y <- 0 until m; x <- 0 until n)
      if (resMatImg(y)(x) < black) {
        val (yMed, xMed): (Int, Int) = 
          transform.Analys.getMediateLine(resMatImg, x, y, directly(y)(x))
        mediateMat(yMed)(xMed) = true
      }
    for (y <- 0 until m; x <- 0 until n) 
      resMatImg(y)(x) = 
        if (!mediateMat(y)(x)) 255
        else 0
      
    val resImgLine = matrixToImage(resMatImg)
    (new MedianFilter).filter(resImgLine, resImgLine)   
    //(new SmartBlurFilter).filter(resImgLine, resImgLine)   
    resList.+=(resImgLine)
    
    resList
  }
  
  def uniWavelet(name: String, a: T, showDirectly: Boolean): ColectionBI = 
    name match {
      case "asVessel" => asVessel(a, showDirectly)
      case "asVesselSpecTransform" => asVesselSpecTransform(a, showDirectly)
      case "morlet" => morlet(a, showDirectly)
    }
  
  def asVessel(a: T, showDirectly: Boolean): ColectionBI = 
    wavelet(transform.AsVessel.transform, a, showDirectly, isWhiteBackground = false)
  def asVesselSpecTransform(a: T, showDirectly: Boolean): ColectionBI  = 
    wavelet(transform.AsVessel.specTransform, a, showDirectly, isWhiteBackground = true)
  
  def morlet(a: T, showDirectly: Boolean): ColectionBI  = 
    wavelet(transform.Morlet.transform, a, showDirectly, isWhiteBackground = false)
}