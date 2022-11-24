package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.tester.generator.regif.BasicTest

class RegIfBasicAccessTester extends AnyFunSuite{
  test("regif_basic_access_tester_apb4"){BasicTest.main(Array("apb4"))}
  test("regif_basic_access_tester_apb3"){BasicTest.main(Array("apb3"))}
}
