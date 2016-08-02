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
