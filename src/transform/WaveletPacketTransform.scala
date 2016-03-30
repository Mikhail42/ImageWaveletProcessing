package transform

import tool.Types._
import tool.ArrayToolKit._

class WaveletPacketTransform(wavelet: WaveletTransformTrait) {
    
  def reverse1D(arrToReverse: A, level: Int): A = {
    val arrTime = new A(arrToReverse.length)
    val k = arrTime.length
    val steps = log2(arrToReverse.length)
    var h = wavelet.transformWavelength << (steps - level)
    while(h <= arrTime.length) {
      val g = k / h // ... -> 8 -> 4 -> 2 -> 1
      val iBuf = new A(h)
      for(p <- 0 until g) {
        copyArray(arrToReverse, p * h, iBuf, 0, h)
        val oBuf = wavelet.waveletReverse(iBuf, h)
        copyArray(oBuf, 0, arrTime, p * h, h)
      } 
      h <<= 1
    } 
    arrTime
  }
  
  def forward1D(arrTime: A, level: Int): A = { 
    val arrHilb = new A(arrTime.length)
    for (l <- 0 until level) {
      val h = arrTime.length >> l
      val iBuf = new A(h)
      for(p <- 0 until (1 << l)) {
        copyArray(arrTime, p * h, iBuf, 0, h)
        val oBuf = wavelet.waveletForward(iBuf, h)
        copyArray(oBuf, 0, arrHilb, p * h, h)
      }
    } 
    arrHilb
  }
}