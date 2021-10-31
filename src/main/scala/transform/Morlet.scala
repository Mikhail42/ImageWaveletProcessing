package transform

import tool.Types._
import math._

object Morlet extends ContinueWavelet {
    
  val k0 = new Pair(-1, 3)
  val eps: T = 8
  val sqrtEps: T = sqrt(eps)
  val epsPowMHalf: T = 1/sqrt(eps)
  
  /** Im-part of wavelet */
  override def psi(xs: Pair): T = 
    normCoef*sin(k0*xs) * exp(-0.5 * Pair(epsPowMHalf*xs.e1, xs.e2).sqrNorm)

  /**
   * A = (y/sqrt(eps) 0)
   * 		 (0           x)
   * detA = x*y/sqzrt(eps)
   * A^(-1) = (1/y       0     )
   * 					(  0	sqrt(eps)/x)
   */
  def fourierPsi(k: Pair): T = {
    val newK = k - k0
    sqrt(eps)*exp(-0.5*Pair(1/newK.e1, sqrtEps/newK.e2).sqrNorm) 
  }
  def fun1ToC_psi(k: Pair): T = fourierPsi(k)/sqrt(k.sqrNorm)
  def funToCP(k: (T, T)): T = sqr(fun1ToC_psi(new Pair(k._1, k._2)))
  
  val C_psi: T = sqr(2*Pi)*
    tool.Integral.simpson2d(funToCP, (-10, -10), (10, 10), 0.1)
  val normCoef: T = 255/sqrt(C_psi)
 // println(255/sqrt(C_psi))
}
