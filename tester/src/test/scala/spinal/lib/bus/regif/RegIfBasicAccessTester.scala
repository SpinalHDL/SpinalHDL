package spinal.lib.bus.regif

import spinal.tester.SpinalAnyFunSuite

class RegIfBasicRtlGenerater extends SpinalAnyFunSuite{
  test("gen_demo_regbank"){BasicTest.genrtl("demo")}
  test("gen_apb4_regbank"){BasicTest.genrtl("apb4")}
  test("gen_apb3_regbank"){BasicTest.genrtl("apb3")}
}

class RegIfBasicAccessTester extends SpinalAnyFunSuite{
  test("regif_basic_access_tester_apb4"){BasicTest.main(Array("apb4"))}
  test("regif_basic_access_tester_apb3"){BasicTest.main(Array("apb3"))}
}

class RegIfStrbTester extends SpinalAnyFunSuite{
  test("apb4_strb_tester"){RegIfStrbTesterSim.sim("all", false)}
  test("apb4_strb_debug"){RegIfStrbTesterSim.sim("debug", true)}
}
