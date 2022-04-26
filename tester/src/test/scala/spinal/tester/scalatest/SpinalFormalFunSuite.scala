package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.idslplugin.Location

class SpinalFormalFunSuite extends AnyFunSuite{  
  def assert(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assert(assertion)
  }

  def assume(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assume(assertion)
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    super.test("formal_" + testName) {
      testFun
    }
  }
}
