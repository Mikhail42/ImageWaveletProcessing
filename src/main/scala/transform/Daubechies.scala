package transform

import tool.Types._

import scala.collection.mutable.ArraySeq

class Daubechies(order: Int) extends WaveletTransformTrait {
  
  val motherWavelength: Int    = order << 1
  val transformWavelength: Int = 2
  override def getScaling: A  = Daubechies.Daub(order)//.par 
}

object Daubechies {
  lazy val Daub = new M(5)
   Daub(1) = Array(
   7.071067811865475244008443621048490392848359376884740365883398e-01, 
   7.071067811865475244008443621048490392848359376884740365883398e-01)//.par
   
   Daub(2) = Array(
   4.829629131445341433748715998644486838169524195042022752011715e-01,
   8.365163037378079055752937809168732034593703883484392934953414e-01,
   2.241438680420133810259727622404003554678835181842717613871683e-01,
  -1.294095225512603811744494188120241641745344506599652569070016e-01)//.par
  
   Daub(3) = Array(
   3.326705529500826159985115891390056300129233992450683597084705e-01,
   8.068915093110925764944936040887134905192973949948236181650920e-01,
   4.598775021184915700951519421476167208081101774314923066433867e-01,
  -1.350110200102545886963899066993744805622198452237811919756862e-01,
  -8.544127388202666169281916918177331153619763898808662976351748e-02,
   3.522629188570953660274066471551002932775838791743161039893406e-02)//.par
  
   Daub(4) = Array(
   2.303778133088965008632911830440708500016152482483092977910968e-01,
   7.148465705529156470899219552739926037076084010993081758450110e-01,
   6.308807679298589078817163383006152202032229226771951174057473e-01,
  -2.798376941685985421141374718007538541198732022449175284003358e-02,
  -1.870348117190930840795706727890814195845441743745800912057770e-01,
   3.084138183556076362721936253495905017031482172003403341821219e-02,
   3.288301166688519973540751354924438866454194113754971259727278e-02,
  -1.059740178506903210488320852402722918109996490637641983484974e-02)//.par
}