package spinal.tester.scalatest

import spinal.core._
import spinal.demo.mandelbrot._

object MandelbrotDemoTester{

}


class MandelbrotDemoTesterBoot extends SpinalTesterBase {
  override def getName: String = "MandelbrotDemo"
  override def createToplevel: Component = new MandelbrotDemo(new MandelbrotCoreParameters(16, 8, 16, 16, 7, 36))

  override def postTest: Unit = {
    super.postTest


    val out = scala.io.Source.fromFile("MandelbrotDemo.out").getLines.reduceLeft(_+_)
    val ref = scala.io.Source.fromFile("tester/src/test/resources/MandelbrotDemo.ref").getLines.reduceLeft(_+_)

    assert(out == ref,"Mandelbrot picture doesn't match with reference")

  }
}