package spinal.tester.scalatest

import spinal.core._
import spinal.tester.scalatest.GrayCounterTester.GrayCounterTester
import language.postfixOps

object GrayCounterTester {
  def grayCounter(n: Int, enable: Bool): UInt = {
    val gray = RegInit(U(0, n bit))
    var even = RegInit(True)
    val word = Cat(True, gray(n-3, 0), even)
    when(enable) {
      var found = False
      for (i <- 0 until n) {
        when(word(i) && !found) {
          gray(i) := !gray(i)
          found \= True
        }
      }
      even := !even
    }
    return gray
  }

  class GrayCounterTester(n: Int) extends Component {
    val enable = in Bool
    val gray = out(grayCounter(n, enable))
  }

}


class GrayCounterTesterGhdlBoot extends SpinalTesterGhdlBase {
  override def getName: String = "GrayCounterTester"

  override def createToplevel: Component = new GrayCounterTester(8)
}

class GrayCounterTesterCocotbBoot extends SpinalTesterCocotbBase {
  override def getName: String = "GrayCounterTester"
  override def pythonTestLocation: String = "tester/src/test/python/spinal/GrayCounterTester"
  override def createToplevel: Component = new GrayCounterTester(8)
}