package transform 

import tool.Types._

trait WaveletTransformTrait {
  
  /** The wavelength of the base or so called mother wavelet and its matching scaling function */
  val motherWavelength: Int
  
  /** The minimal wavelength of a signal that can be transformed */
  val transformWavelength: Int
  
  def getScaling: A 
  val scalingDeCom: A = getScaling
  lazy val waveletDeCom: A = {
    val res = new A(motherWavelength)
    for (i <- 0 until motherWavelength)
      res(i) = if ((i&1)==0) scalingDeCom(motherWavelength - 1 - i)
               else         -scalingDeCom(motherWavelength - 1 - i)
    res
  }
   
  def scaling(j: Int): T = scalingDeCom(j)
  def wavelet(j: Int): T = waveletDeCom(j) 
  
  /** @param id:
   * 		1 -- Reverse
   * 	  2 -- Forward
	 */
  def waveletTransform(array1: A, lengthAr: Int, id: Int): A = {
    val array2 = new A(lengthAr)
    val h = lengthAr >> 1  
    for(i <- 0 until h; j <- 0 until motherWavelength) {
      val k = (( i << 1 ) + j) % lengthAr 
      if (id == 1){
        array2(i)     += array1(k) * scaling(j) // low pass filter for the energy (approximation)
        array2(i + h) += array1(k) * wavelet(j) // high pass filter for the details
      } else { //if (id == 2)
        // adding up energy from low pass (approximation) and details from high pass filter
        array2(k) += array1(i)   * scaling(j) +
                     array1(i+h) * wavelet(j)
      }
    } 
    array2
  }  
  
  def waveletReverse(arrHilb: A, arrHilbLength: Int): A = 
    waveletTransform(arrHilb, arrHilbLength, 1)
  
  def waveletForward(arrTime: A, arrTimeLength: Int): A = 
    waveletTransform(arrTime, arrTimeLength, 2)
}