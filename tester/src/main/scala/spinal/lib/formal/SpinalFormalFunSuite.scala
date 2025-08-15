package spinal.lib.formal

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Tag
import spinal.core._
import spinal.core.formal._
import spinal.idslplugin.Location

object SpinalFormal extends Tag("spinal.tester.formal")
object SpinalFormalPSL extends Tag("spinal.tester.psl")

class SpinalFormalFunSuite extends AnyFunSuite{
  implicit val className: String = getClass.getSimpleName()
  def assert(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assert(assertion)
  }

  def assume(assertion: Bool)(implicit loc: Location) = {
    spinal.core.assume(assertion)
  }

  def test(testName: String)(testFun: => Unit): Unit = {
    if (testName.toLowerCase.contains("ghdl")) {
      super.test("formalpsl_" + testName, SpinalFormalPSL) {
        testFun
      }
    } else {
      super.test("formal_" + testName, SpinalFormal) {
        testFun
      }
    }
  }

  def shouldFail(body: => Unit) = assert(try {
    body
    false
  } catch {
    case e : Throwable => println(e); true
  })

  def shouldFailWithOutput(output: String)(testFun: => Unit): Unit = {
    import java.io.ByteArrayOutputStream
    val stdoutCapture = new java.io.ByteArrayOutputStream
    Console.withOut(stdoutCapture) {
      shouldFail(testFun)
    }
    println(stdoutCapture)
    assert(stdoutCapture.toString.contains(output))
  }
}
