package spinal.tester.scalatest

import spinal.core._
import spinal.demo.mandelbrot._

class MandelbrotTester(p : MandelbrotCoreParameters ) extends MandelbrotCore(p){
  print("miaou")
}


//class MandelbrotTesterGhdlBoot extends SpinalTesterGhdlBase {
//  override def getName: String = "MandelbrotTester"
//  override def createToplevel: Component = new MandelbrotTester(MandelbrotCoreParameters(16, 8, 16, 16, 7, 34))
//  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
//    config.copy(defaultClockDomainFrequency=FixedFrequency(100 MHz))
//  }
//  override def postTest: Unit = {
//    super.postTest
//
//    val out = scala.io.Source.fromFile("MandelbrotTester.out").getLines.reduceLeft(_+_)
//    val ref = scala.io.Source.fromFile("tester/src/test/resources/MandelbrotTester.ref").getLines.reduceLeft(_+_)
//    assert(out == ref,"Mandelbrot picture doesn't match with reference")
//
//  }
//}


class MandelbrotTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "MandelbrotTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/MandelbrotTester"
  override def createToplevel: Component = new MandelbrotTester(MandelbrotCoreParameters(16, 8, 16, 16, 7, 34))
  override def backendConfig(config: SpinalConfig) : SpinalConfig = {
    config.copy(defaultClockDomainFrequency=FixedFrequency(100 MHz))
  }
}