package spinal.tester.scalatest

import spinal.core._
import spinal.demo.mandelbrot._

class MandelbrotTester(p : MandelbrotCoreParameters ) extends MandelbrotCore(p){
  "miaou"
}


class MandelbrotTesterBoot extends SpinalTesterBase {
  override def getName: String = "MandelbrotTester"
  override def createToplevel: Component = new MandelbrotTester(MandelbrotCoreParameters(16, 8, 16, 16, 7, 34))
  override def backendConfig(config: SpinalVhdl[_]) : Unit = {
    super.backendConfig(config)
    config.setDefaultClockFrequency(FixedFrequency(100e6))
  }
  override def postTest: Unit = {
    super.postTest

    val out = scala.io.Source.fromFile("MandelbrotTester.out").getLines.reduceLeft(_+_)
    val ref = scala.io.Source.fromFile("tester/src/test/resources/MandelbrotTester.ref").getLines.reduceLeft(_+_)
    assert(out == ref,"Mandelbrot picture doesn't match with reference")

  }
}