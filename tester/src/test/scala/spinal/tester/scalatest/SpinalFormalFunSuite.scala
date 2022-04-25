package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._

class SpinalFormalFunSuite extends AnyFunSuite{  
  def assert(target: Bool) = {
    spinal.core.assert(target)
  }

  def assume(target: Bool) = {
    spinal.core.assume(target)
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("formal_" + testName) {
      testFun
    }
  }
}
