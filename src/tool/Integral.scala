package tool

import Types._

object Integral {
  
  /** return min n in 2*N+1: (x2-x1)/h0 <= n */
  def getN(x1: T, x2: T, h0: T): Int = ((((x2 - x1)/h0).ceil.toInt >> 1) << 1) + 1
  
  /** @see "Simpsons rule" in Internet */
  def simpson(f: T => T, x1: T, x2: T, h0: T): T = {
    val n = getN(x1, x2, h0)
    val h = (x2-x1)/n
    var sum1: T = 0
    for (x <- x1+h until x2 by 2*h)
      sum1 += f(x)
    sum1 *= 4
    var sum2: T = 0
    for (x <- x1+2*h until x2 by 2*h)
      sum2 += f(x)
    sum2 *= 2
    (h/3)*(sum1 + sum2 + f(x1) + f(x2))
  }
  
  /** Simpsons rule for 2D 
   *  @see simpson 
   **/
  def simpson2(f: ((T, T)) => T, x1: (T, T), x2: (T, T), h0: T): T = {
    val n = getN(x1._1, x2._1, h0)
    val h1 = (x2._1-x1._1)/n
    var res: T = 0
    for (x <- x1._1 to x2._1 by h1) {
      def g: T => T = f(x, _)
      res += simpson(g, x1._2, x2._2, h0)
    }
    res
  }
}