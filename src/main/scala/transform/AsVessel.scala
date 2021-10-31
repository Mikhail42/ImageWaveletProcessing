package transform

import tool.Types._
import math._

object AsVessel extends ContinueWavelet {
  
  /** width of transform */
  val d: T = 5
  
  /** wavelet for blood vessel  */
  override def psi(xs: Pair): T = 
    if (abs(xs.e1) <= d) psi(xs.e2)  
    else       0
  /** (1-x^4) * exp(- x^4 / 4) */
  protected def psi(x: T): T = (1-pow4(x))*exp(-0.25*pow4(x))
  
  /** integral of wavelet */ 
  protected def intPsi(x: T): T = x*exp(-0.25*pow4(x)) 
  /** @see intPsi(x2) - intPsi(x1) */
  protected def integralPsi(x1: T, x2: T): T = intPsi(x2) - intPsi(x1)
  override def integralPsi(x1: Pair, x2: Pair): T = 
    if (x1.e2 <= -d || x1.e1 > d) 0   
    else integralPsi(max(x2.e1, -d), min(x2.e2, d))   
  
  /** transform with specified norms */
  override def specTransform(mat: M, a: T): M = {
    val m = mat.length; val n = mat(0).length 
    // size of mask 
    val sy = (sy0*a).ceil.toInt; val sx = (sx0*a).ceil.toInt
    val amountElemMat: T = (2*sy+1)*(2*sx+1); 
    //val invAmount: T = 1.0 / amountElemMat
    
    /** mean mat(-sy+y:y+sy, -sx+x:x+sx) 
     *  for (x > sx) 
     **/ 
    def mx(lastMX: T, y: Int, x: Int): T = {
      var res = lastMX*amountElemMat
      val xPsx = x+sx; val xMsx = x-sx-1;
      for (ky <- -sy+y to y+sy) 
        res += (mat(ky)(xPsx) - mat(ky)(xMsx))
      res/amountElemMat//*invAmount
    }
    
    /** mean mat(-sy+y:y+sy, -sx+x:x+sx) 
     *  for (x == sx) 
     **/
    def simpleMXF(y: Int, x: Int): T = {
      var sum: T = 0
      for (ky <- -sy+y to y+sy; kx <- -sx+x to x+sx) 
        sum += mat(ky)(kx)
      sum/amountElemMat//*invAmount
    }
    
    var lastMXF = simpleMXF(sy, sx)
    var curMXF = simpleMXF(sy, sx)
    
    /** eps2 mat(-sy+y:y+sy, -sx+x:x+sx) 
     *  for (x > sx) 
     **/
    def getEps2(lastEPS2: T, y: Int, x: Int): T = {
      var res = lastEPS2
      curMXF = mx(lastMXF, y, x) 
      val xPsx = x+sx; val xMsx = x-sx-1;
      for (ky <- -sy+y to y+sy)
        res += (sqr(mat(ky)(xPsx) - curMXF) - sqr(mat(ky)(xMsx) - lastMXF))
      lastMXF = curMXF
      res
    }
    
    /** eps2 mat(-sy+y:y+sy, -sx+x:x+sx) 
     *  for (x == sx) 
     **/
    def simpleEPS2(y: Int, x: Int): T = {
      var sum: T = 0
      curMXF = simpleMXF(y, x)
      for (ky <- -sy+y to y+sy; kx <- -sx+x to x+sx)
        sum += sqr(mat(ky)(kx) - curMXF)
      lastMXF = curMXF
      sum
    }
    var lastEPS2: T = simpleEPS2(sy, sx)
    
    def initDeltaPsi(x: Int): T = integralPsi((x+0.5)/a, (x-0.5)/a)
    val deltaPsi = for (x <- 0 until n) yield initDeltaPsi(x)
    val integPsi05 = integralPsi(0.5/a, -0.5/a)
    
    val res: M = createM(m, n)
    for (y <- sy until m-sy; x <- sx until n-sx) {
      lastEPS2 = 
        if (x == sx) simpleEPS2(y, x)
        else getEps2(lastEPS2, y, x)

      // epsilon in power 2
      val eps2: T = lastEPS2
      val sumR0s: T = 
        (-sy+y to y+sy).map{mat(_)(x)}.sum*
        integPsi05
      val tX = x+x
      
      var sum: T = sumR0s  
      for (kx <- -sx+x to x+sx) {
        var inSum: T = 0
        for (ky <- -sy+y to y+sy)
          inSum += mat(ky)(kx) + mat(ky)(tX-kx)
        sum += inSum*deltaPsi(kx)
      }
        
      /*
      for (ky <- -sy+y to y+sy) {
        var inSum: T = 0
        for (kx <- -sx+x to x+sx)
          inSum += (mat(ky)(kx) + mat(ky)(tX-kx))*deltaPsi(kx)
        sum += inSum
      }*/
      res(y)(x) = sum / (a*sqrt(eps2))
    }
    res
  }
}  