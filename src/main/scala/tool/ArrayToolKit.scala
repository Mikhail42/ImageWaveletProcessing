package tool

import Types._

object ArrayToolKit {
  def copyColumn(mat: M, j: Int): A = mat.map {_(j)}
  
  def copyColumn(ar: A, mat: M, j: Int): Unit = 
    for (i <- 0 until mat.length) mat(i)(j) = ar(i) 
  
  /** @see Array.copy */
  def copyArray[E](ar1: Array[E], pos1: Int, ar2: Array[E], pos2: Int, length: Int) {
    val delta = pos1 - pos2
    for (i <- pos2 until pos2+length) ar2(i) = ar1(i+delta)
  }
  
  
  /** @see Array.copy */
  def copyArray(ar1: A, pos1: Int, ar2: A, pos2: Int, length: Int) {
    val delta = pos1 - pos2
    for (i <- pos2 until pos2+length) ar2(i) = ar1(i+delta)
  }
}