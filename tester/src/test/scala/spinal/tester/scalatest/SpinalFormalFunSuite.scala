package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._

class SpinalFormalFunSuite extends AnyFunSuite{  
  def assert(assertion: Bool) = {
    spinal.core.assert(assertion)
  }

  def assume(assertion: Bool) = {
    spinal.core.assume(assertion)
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("formal_" + testName) {
      testFun
    }
  }
}
