package tool

import Types._

object ArrayToolKit {
  def copyColumn(mat: M, j: Int): A = mat.map(_(j))
  
  def copyColumn(ar: A, mat: M, j: Int): Unit =
    for (i <- 0 until mat.length) mat(i)(j) = ar(i) 
  
  /** @see Array.copy */
  def copyArray[E](ar1: Array[E], pos1: Int, ar2: Array[E], pos2: Int, length: Int): Unit =
    java.lang.System.arraycopy(ar1, pos1, ar2, pos2, length)
  
  /** @see Array.copy */
  def copyArray(ar1: A, pos1: Int, ar2: A, pos2: Int, length: Int): Unit =
    java.lang.System.arraycopy(ar1, pos1, ar2, pos2, length)
}