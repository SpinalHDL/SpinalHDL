package spinal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

package object tester {
  val simWorkspacePath = "./simWorkspace"
  class SpinalAnyFunSuite extends AnyFunSuite {
    SpinalConfig.defaultTargetDirectory = simWorkspacePath
  }
  type SpinalTesterCocotbBase = scalatest.SpinalTesterCocotbBase
  type SpinalTesterGhdlBase = scalatest.SpinalTesterGhdlBase
  type SpinalFormalFunSuite = lib.formal.SpinalFormalFunSuite
}
