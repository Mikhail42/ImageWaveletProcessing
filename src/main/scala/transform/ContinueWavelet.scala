package transform

import tool.Types._
import tool.Integral
import image.Operation
import math._

abstract class ContinueWavelet {
  
  /** half-width of the height window */
  protected def sy0: Int = 5
  /** half-width of the widht window */
  protected def sx0: Int = 5
  def toTrueSize(s0: Int, a: T) = (s0*a).ceil.toInt
  /** wavelet */
  def psi(xs: Pair): T 
  
  /** integration's step for wavelet transform */
  protected def h: T = 0.1
  /** approximation integral of wavelet with step #h */
  protected def integralPsi(x1: Pair, x2: Pair): T = simpson(x1.toP, x2.toP)
  protected def simpson(x1: P, x2: P): T = 
    Integral.simpson2d((p: P) => psi(pToPair(p)), x1, x2, h)
 
  protected def psiMatrix(mat: M, a: T): M = {
    val sx = toTrueSize(sx0, a); val sy = toTrueSize(sy0, a)
    val res = createM(2*sy+1, 2*sx+1)
    for (y <- -sy to sy; x <- -sx to sx)
      res(y+sy)(x+sx) = integralPsi(Pair(y,x)/a, Pair(y+1,x+1)/a)/sqrt(a)
    res
  }
  
  /** wavelet transform */
  def transform(mat: M, a: T): M = { 
    val psiMat = psiMatrix(mat, a)
    transform(mat, a, psiMat)
  }
  
  protected def transform(mat: M, a: T, psiMat: M): M = {
    val m = mat.length;   val n = mat(0).length
    val sx = toTrueSize(sx0, a); val sy = toTrueSize(sy0, a)
    
    val res = createM(m, n)
    for (y <- 0 until m; x <- 0 until n){
      var sum: T = 0
      /** mink+y>=0 & maxk+y<m  */
      val mink = if (y < sy)    -y     else -sy
      val maxk = if (y > m-1-sy) m-1-y else  sy
      val minl = if (x < sx)    -x     else -sx
      val maxl = if (x > n-1-sx) n-1-x else  sx
      for (k <- mink to maxk; l <- minl to maxl)
        sum += mat(k+y)(l+x) * psiMat(k+sy)(l+sx)
      res(y)(x) = sum
    }
    res
  }
   
  /** transform with specified norms */
  def specTransform(mat: M, a: T): M = ??? 
  
  def transformWithDirectoryField(mat: MInt, a: T, isAll: Boolean = false): (M, M) = {        
    val m = mat.length; val n = mat(0).length
    val directlyRes:  M = createWhiteMat(m, n)
    val transformRes: M = createWhiteMat(m, n)
    
    // max_{theta} {W(b,a, theta)}
    val m0 = 3*m/2; val n0 = 3*n/2
    for (theta <- 0 until 180 by 20) {
      val matToTrans: M = Operation.rotateFromMInt(mat, theta, m, n)
      val trG: M = specTransform(matToTrans, a)
      val reverse: M    = Operation.rotate(trG, -theta, 2*m, 2*n)
      for (i <- m0 until m0+m; j <- n0 until n0+n)
        if (transformRes(i-m0)(j-n0) > abs(reverse(i)(j))) {
          directlyRes(i-m0)(j-n0) = theta
          transformRes(i-m0)(j-n0) = abs(reverse(i)(j))
        } 
    }
    
    // line
    if (isAll) {
      val widthOfLine = 21; val halfW = widthOfLine/2
      for (y <- halfW until m-halfW; x <- halfW until n-halfW) {
        val theta: T = directlyRes(y)(x)
        val angle: T = Pi*((theta-90)/180)
        // direction of search line: dy = k*dx = tg*dx
        val tg: T = tan(angle)
        // get dy using dx and direction
        def getNewY(dx: Int): Int = (dx*tg).floor.toInt.max(-y).min(m-y-1)
        // search line
        val line: IndexedSeq[T] = 
          for (dx <- -halfW to halfW) 
            yield transformRes(y+getNewY(dx))(x+dx)
        val filterLine = 
          for (j <- 1 until line.length-1) 
            yield (line(j-1)+line(j)+line(j+1)) 
        // index on line with max value of green component
        val ind = filterLine.indexWhere(_ == filterLine.max)+1
        // distance to point (x, y) 
        val dx = -halfW+ind; val dy = getNewY(dx)
        if (abs(dx)+abs(dy) > 5) 
          directlyRes(y)(x) = 0
      }
    }
    (transformRes, directlyRes)
  }
}