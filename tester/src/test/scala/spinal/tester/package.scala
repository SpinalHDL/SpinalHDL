package spinal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

package object tester {
  val simWorkspacePath = "./simWorkspace"
  class SpinalAnyFunSuite extends AnyFunSuite {
    SpinalConfig.defaultTargetDirectory = simWorkspacePath
  }
}
