package transform

import tool.Types._
import math._

import image.ImageWaveletInterface.black

object Analys {
  
  private def toInt(x: T) = x.round.toInt
  
  /**
   * @param imgMat -- components of image, access to element in format (h,w)
   * @param angle -- (directly of blood vessel - pi/2), in [-pi/2, pi/2).
   * @param x -- current value pixel's width
   * @param y -- current value pixel's height 
   * @return (r1, r2), 
   * 	where #r1 -- radius of the motion in the direction of the #angle 
   * 		from the point (#x, #y) until the last black dot in, #r2 -- in the direction of the (#angle+Pi). 
   */
  private def blackLine(imgMat: M, angle: T, x: Int, y: Int): (Int, Int) = {
    val m = imgMat.length; val n = imgMat(0).length
  
    /** @see image.ImageWaveletInterface.black */
    def isBlack(x: Int, y: Int) = imgMat(y)(x) < black 
    def inMatrix(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && x < n && y < m
    def r(angle: T): Int = {
      var r = 0
      val cosA = cos(angle); val sinA =  sin(angle)
      var kx = x; var ky = y
      while (inMatrix(kx, ky) && isBlack(kx, ky)) {
        kx = x+toInt(r*cosA)
        ky = y+toInt(r*sinA) 
        r += 1
      } 
      r
    }
    
    (r(angle)-1, r(angle+Pi)-1)
  }
  
  /**
   * @param imgMat -- components of image, access to element in format (h,w)
   * @param theta -- directly of line, in [0, 180).
   * @param x -- current value pixel's width
   * @param y -- current value pixel's height
   * @return (yMediate, xMediane) -- coordiane of mediate black line: 
   * 		(theta - 90[degree] = directly(line)) & ((y,x) in line)
   */
  def getMediateLine(imgMat: M, x: Int, y: Int, theta: T): (Int, Int) = {
    var r1 = 100; var r2 = 100; var t = theta;
    //for (t_ <- theta-60 to theta+60 by 3.0){
    (-30+theta until theta+30 by 1.0).par.foreach(t_ => {
      val angle: T = Pi*(t_ - 90)/180
      val (r1_, r2_) = blackLine(imgMat, angle, x, y)
      if (r1_ + r2_ < r1+r2) {
        r1 = r1_; r2 = r2_; t = t_
      }
    })
    val angle: T = Pi*(t - 90)/180
    val cosA = cos(angle); val sinA =  sin(angle)
    
    val m = imgMat.length; val n = imgMat(0).length
    def inMatrix(x: Int, y: Int) = (x >= 0 && y >= 0 && x < n && y < m)
        
    val xMed = x + toInt((r1-r2)*cosA/2)
    val yMed = y + toInt((r1-r2)*sinA/2)
    (yMed, xMed)
  }
}