package transform 

import tool.ArrayToolKit._
import tool.Types._

/**
 *  @author  Mikhail   Ionkin     (ionkinmikhail@gmail.com)
 *  
 *	I'm using a project JWave (by Christian Scheiblich (cscheiblich@gmail.com))
 *  @see	https://github.com/cscheiblich/JWave
 */
trait TransformTrait {
  
  /** [1]:
   * Performs the reverse transform from frequency or Hilbert domain to time
   * domain for a given array depending on the used transform algorithm by
   * inheritance.
   */ 
  def reverse1D(arrToReverse: A): A
  /** @see reverse1D(arrToReverse) */
  def reverse1D(arrToReverse: A, level: Int): A
  
  /** [1]: 
   * Performs the forward transform from time domain to frequency or Hilbert
   * domain for a given array depending on the used transform algorithm by
   * inheritance. 
   */
  def forward1D(arrToForward: A): A 
  /** @see forward1D(arrToForward) */
  def forward1D(arrToForward: A, level: Int): A
  
  private def two(mat1: M, lvlM: Int, lvlN: Int, fun1D: (A, Int) => A): M = {
    val mat2 = for(str <- mat1) yield fun1D(str, lvlN)
    val mat3 = createM(mat2.length, mat2(0).length)
    for(j <- 0 until mat1(0).length) 
      copyColumn(fun1D(copyColumn(mat2, j), lvlM), mat3, j)
    mat3
  }
  
  /**
   * @param transformID: 
   * 		str -- transform of strings
   * 		col -- transform of columns
   *    mat -- transform of strings and columns
   */
  def two(mat1: M, lvlM: Int, lvlN: Int, fun1D: (A, Int) => A, transformID: String): M = 
    transformID match {
      case "str" =>
        for(str <- mat1) yield fun1D(str, lvlN)
      case "col" =>{
        val res = createM(mat1.length, mat1(0).length)
        for(j <- 0 until mat1(0).length) 
          copyColumn(fun1D(copyColumn(mat1, j), lvlM), res, j)
        res
      }
      case "mat" => {
        val mat2 = for(str <- mat1) yield fun1D(str, lvlN)
        val mat3 = createM(mat1.length, mat1(0).length)
        for(j <- 0 until mat1(0).length) 
          copyColumn(fun1D(copyColumn(mat2, j), lvlM), mat3, j)
        mat3
      }
    }
  
  /** [1] 
   * Performs the 2-D forward transform from time domain to frequency or Hilbert
   * domain for a given matrix depending on the used transform algorithm by
   * inheritance.
   */
  def forward2D(matTime: M): M = forward2D(matTime, log2(matTime.length), log2(matTime(0).length))
  
   /** @see forward2D(matTime) */
  def forward2D(matTime: M, transformID: String): M =
    forward2D(matTime, log2(matTime.length), log2(matTime(0).length), transformID)
  
  /** @see forward2D(arrToForward) */
  def forward2D(matTime: M, lvlM: Int, lvlN: Int): M = two(matTime, lvlM, lvlN, forward1D)
  
  /** @see forward2D(arrToForward) */
  def forward2D(matTime: M, lvlM: Int, lvlN: Int, transformID: String): M =
    two(matTime, lvlM, lvlN, forward1D, transformID)
  
  
  /**  [1]
   * Performs the 2-D reverse transform from frequency or Hilbert or time domain
   * to time domain for a given matrix depending on the used transform algorithm
   * by inheritance.
   */
  def reverse2D(matFreq: M): M = reverse2D(matFreq, log2(matFreq.length), log2(matFreq(0).length))
  
  /** @see reverse2D(arrToReverse) */
  def reverse2D(matFreq: M, lvlM: Int, lvlN: Int): M = two(matFreq, lvlM, lvlN, reverse1D)
  
  /** @see reverse2D(matFreq) */
  def reverse2D(matFreq: M, transformID: String): M = 
    reverse2D(matFreq, log2(matFreq.length), log2(matFreq(0).length), transformID)
  
  /** @see reverse2D(arrToReverse) */
  def reverse2D(matFreq: M, lvlM: Int, lvlN: Int, transformID: String): M =
    two(matFreq, lvlM, lvlN, reverse1D, transformID)
}