package spinal.tester.scalatest

import spinal.core._
import spinal.tester.scalatest.GrayCounterTester.GrayCounterTester
import language.postfixOps

object GrayCounterTester {

  class GrayCounterTester(n: Int) extends Component {
    val enable = in Bool
    val gray = out(grayCounter(n, enable))
  }

}


class GrayCounterTesterBoot extends SpinalTesterBase {
  override def getName: String = "GrayCounterTester"

  override def createToplevel: Component = new GrayCounterTester(8)
}