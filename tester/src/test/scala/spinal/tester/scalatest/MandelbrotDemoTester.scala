package spinal.tester.scalatest

import spinal.core._
import spinal.demo.mandelbrot._

object MandelbrotDemoTester{

}


class MandelbrotDemoTesterBoot extends SpinalTesterBase {
  override def getName: String = "MandelbrotDemo"
  override def createToplevel: Component = new MandelbrotDemo(new MandelbrotCoreParameters(64, 4, 64, 64, 7, 36))
}