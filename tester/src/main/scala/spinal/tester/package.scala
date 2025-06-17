package spinal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

package object tester {
  val simWorkspacePath = "./simWorkspace"
  class SpinalAnyFunSuite extends AnyFunSuite {
    def shouldFail(body: => Unit) = assert(try {
      body
      false
    } catch {
      case e : Throwable => println(e); true
    })
    SpinalConfig.defaultTargetDirectory = simWorkspacePath
  }
  type SpinalTesterCocotbBase = scalatest.SpinalTesterCocotbBase
  type SpinalTesterGhdlBase = scalatest.SpinalTesterGhdlBase
  type SpinalFormalFunSuite = lib.formal.SpinalFormalFunSuite
}
