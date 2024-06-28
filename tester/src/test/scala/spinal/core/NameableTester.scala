package spinal.core

import org.scalatest.funsuite.AnyFunSuite

object NameableTester {
  class NameableTester(nameToSet: String) extends Component {
    val signal = Bool().setName(nameToSet)
  }
}

class NameableTester extends AnyFunSuite {
  def genVhdl(nameToSet: String): Unit = {
    SpinalConfig(mode = VHDL).generate(new NameableTester.NameableTester(nameToSet))
  }
  def genVhdlShouldFail(nameToSet: String): Unit = {
    try {
      genVhdl(nameToSet)
    } catch {
      case e: Throwable => {
        return
      }
    }
    assert(false, s"VHDL generation for name '$nameToSet' was expected to fail, but did not :(")
  }

  def genVerilog(nameToSet: String): Unit = {
    SpinalConfig(mode = Verilog).generate(new NameableTester.NameableTester(nameToSet))
  }
  def genVerilogShouldFail(nameToSet: String): Unit = {
    try {
      genVerilog(nameToSet)
    } catch {
      case e: Throwable => {
        return
      }
    }
    assert(false, s"Verilog generation for name '$nameToSet' was expected to fail, but did not :(")
  }

  test("vhdl - invalid formats") {
    genVhdlShouldFail("_test_abc")
    genVhdlShouldFail("1test_abc")
    genVhdlShouldFail("test_abc_")
    genVhdlShouldFail("test__abc")
    genVhdlShouldFail("test abc")
    genVhdlShouldFail("test@abc")
  }

  test("vhdl - valid formats") {
    genVhdl("test_abc")
    genVhdl("test_abc1")
    genVhdl("t1est_abc")
    genVhdl("t_e_s_t_a_b_c")
  }

  test("verilog - invalid formats") {
    genVerilogShouldFail("1test_abc")
    genVerilogShouldFail("test abc")
    genVerilogShouldFail("test@abc")
    genVerilogShouldFail("$test_abc")
  }

  test("verilog - valid formats") {
    genVerilog("test_abc")
    genVerilog("test_abc1")
    genVerilog("t1est_abc")
    genVerilog("t_e_s_t_a_b_c")
    genVerilog("test_abc$")
    genVerilog("_test_abc")
    genVerilog("t$est_abc")
  }
}