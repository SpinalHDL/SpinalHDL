package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._

import scala.sys.process._

class ChecksTester extends FunSuite  {

  def checkFailure(func : => Unit) : Boolean = {
    try{func} catch {
      case e: Throwable => {
        print(e)
        return true
      }
    }
    return false
  }
  def generationShouldFaild(gen : => Component): Unit ={
    assert(checkFailure{SpinalVhdl(gen)})
    assert(checkFailure{SpinalVerilog(gen)})
  }

  def generationShouldPass(gen : => Component): Unit ={
    assert(!checkFailure{SpinalVhdl(gen)})
    assert(!checkFailure{SpinalVerilog(gen)})
  }

  test("reflectionNamming") {
    val t = SpinalVhdl(new Component{
      val a = new Area{
        val aa = Bool
        val bb = new Area{
          val aaa = Bool
          val bbb = Vec(Bool,4)
          val ccc = Vec(new Bundle{
            val aaaa = Bool
            val bbbb = Vec(Bool,8)
            val cccc = Vec( Vec( Vec(Bool,8),8),8)
            val dddd = List.fill(4)(Bool)
            val eeee = List.fill(4)(List.fill(4)(Bool))
          },4)
        }
      }
      val b = Bool
    }).toplevel

    assert(t.b.getName() == "b")
    assert(t.a.aa.getName() == "a_aa")
    assert(t.a.bb.aaa.getName() == "a_bb_aaa")
    assert(t.a.bb.bbb(2).getName() == "a_bb_bbb_2")
    assert(t.a.bb.ccc(3).aaaa.getName() == "a_bb_ccc_3_aaaa")
    assert(t.a.bb.ccc(3).bbbb(6).getName() == "a_bb_ccc_3_bbbb_6")
    assert(t.a.bb.ccc(3).cccc(6)(5)(4).getName() == "a_bb_ccc_3_cccc_6_5_4")
    assert(t.a.bb.ccc(3).dddd(3).getName() == "a_bb_ccc_3_dddd_3")
    assert(t.a.bb.ccc(3).eeee(3)(2).getName() == "a_bb_ccc_3_eeee_3_2")
  }

  test("checkWidthAssignment") {
    generationShouldFaild(new Component{
      val output = out Bits(8 bits)
      output := "00"
    })
  }


  test("checkCombinatorialLoop") {
    generationShouldFaild(new Component{
      val input = in Bits(8 bits)
      val cond = in Bool

      val tempA = Bits(4 bits)
      val output = out Bits(8 bits)

      tempA := input(7 downto 4)
      val tempB = Bits(4 bits)
      tempB := tempA

      when(cond){
        tempA(1) := tempB(0)
      }

      output(3 downto 0) := input (3 downto 0)
      output(7 downto 4) := tempB
    })
  }

  test("checkNoPartialAssignement") {
    generationShouldPass(new Component{
      val cond = in Bool
      val input = in Bits(8 bits)
      val output = out Bits(8 bits)
      when(cond){
        output(7 downto 1) := input.resized
        when(cond){
          output(0) := cond
        } otherwise{
          output(0) := cond
        }
      }otherwise{
        output := input
      }
    })

    generationShouldFaild(new Component{
      val cond = in Bool
      val input = in Bits(8 bits)
      val output = out Bits(8 bits)
      when(cond){
        output(7 downto 1) := input.resized
        when(cond){

        } otherwise{
          output(0) := cond
        }
      }otherwise{
        output := input
      }
    })
  }

  test("checkClockCrossing") {
    generationShouldFaild(new Component{
      val clockA = in Bool
      val clockB = in Bool

      val areaA = new ClockingArea(ClockDomain(clockA)){
        val reg = Reg(Bool)
        reg := in(Bool)
      }

      val areaB = new ClockingArea(ClockDomain(clockB)){
        val reg = Reg(Bool)
        reg := areaA.reg
        val output = out Bool()
        output := reg
      }
    })
  }

  test("checkNoInputWrite") {
    generationShouldFaild(new Component{
      val input = in Bool()
      val output = out Bool()
      output := input
      input := False
    })
  }


}
