package tool

import scala.collection.parallel.immutable.ParVector

object Types {
  type T = Double
  type Ar[Y] = Array[Y]
  type A = Ar[T]
  type AInt = Ar[Int]
  type ABool = Ar[Boolean]
  type M = Ar[A]  
  type MInt = Ar[AInt]
  type MBool = Ar[ABool]
  type BI = java.awt.image.BufferedImage
  type ColectionBI = scala.collection.mutable.ArrayBuffer[BI]
  
  def createWhiteMat(m: Int, n: Int): M = Array.fill[T](m, n)(255)
  def createM(m: Int, n: Int): M = Array.fill[T](m, n)(0)
  def createMInt(m: Int, n: Int): MInt = Array.fill[Int](m, n)(0)
  def createMBool(m: Int, n: Int): MBool = Array.fill[Boolean](m, n)(false)
  
  def sqr(x: T) = x*x
  def pow4(x: T) = sqr(sqr(x))
  def log2(number: Int): Int = {
    var res = 0
    while ((number >> res) > 0) res +=1
    (res-1)
  }
    
  type P = (T, T)
  def pairToP = (p: Pair) => (p.e1, p.e2)
  def pToPair = (p: P) => new Pair(p._1, p._2)
  case class Pair(e1: T, e2: T){
    def * (p: Pair): T = e1*p.e1 + e2*p.e2
    def * (z: T): Pair = Pair(e1*z, e2*z)
    def * (ar: ArPair): Pair = 
      Pair(e1*ar.p1.e1 + e2*ar.p2.e1,
           e1*ar.p1.e2 + e2*ar.p2.e2)
    def / (z: T)    = this*(1/z)
    def sqrNorm: T  = this*this
    def - (p: Pair) = Pair(e1-p.e1, e2-p.e2)
    def toP = (e1, e2)
  }
  
  case class ArPair(p1: Pair, p2: Pair){
    def * (p: Pair) = Pair(p1*p, p2*p)
    def / (e: T)    = ArPair(p1/e, p2/e)
  }
}