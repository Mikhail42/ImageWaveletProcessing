package test

import tool.Types._
   
object Test {
  def test(waveName: String): Unit = {
    // directory of images
    val dir: String = "src/main/resources/images/" //generate Image/"
    // image name in directory, without format. 
    // if a full image name without format, it is don't work
    val imgName: String = "01_dr"//"outGenerateImg"
    // image format
    val format: String = "jpg"
    
    val img: BI = image.Input.getImage(dir+imgName+"."+format) 
    val iwi = new image.ImageWaveletInterface(img)
    // run test with using wavelet, according with vessel model
    val a: T = 0.6
    //for (a <- 0.2 to 2 by 0.3)
      val list = iwi.uniWavelet(waveName, a, showDirectly = true)
      var i = 0
      for (el <- list) {
        image.Output.saveImage(el, dir+i+i+'.'+format, format)    
        i += 1
      }
  }
}