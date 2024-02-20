package spinal.tester

import spinal.core._

package object scalatest {
  val simWorkspacePath = spinal.tester.simWorkspacePath
  type SpinalAnyFunSuite = spinal.tester.SpinalAnyFunSuite
  type SpinalSimFunSuite = spinal.tester.SpinalSimFunSuite
  object SpinalSimTester{
    def apply(body :  => spinal.tester.SpinalSimTester => Unit): Unit = {
      spinal.tester.SpinalSimTester(body)
    }
  }
}
