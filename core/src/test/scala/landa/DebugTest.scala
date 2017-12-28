package landa

import spinal.core._
import spinal.sim._
import spinal.core.sim._



object DebugTest {

  import scala.util.continuations.cpsParam
  //@cpsParam[Unit,Unit] is equivalent to @suspendable
  def subFunc() : Unit@cpsParam[Unit,Unit] = {}

  //Compilation fail, error message is :
//  Error:(13, 6) type mismatch;
//  found   : Unit
//  required: Unit @scala.util.continuations.cpsParam[Unit,Unit]
//  if(cond){
//  def funcA(cond : Boolean) : Unit@cpsParam[Unit,Unit] = {
//    if(cond){  //This is where the error is pointing
//      subFunc()
//    }
//    subFunc()
//  }

  //Compilation OK
  def funcB(cond : Boolean) : Unit@cpsParam[Unit,Unit] = {
    val dummy = if(cond){
      subFunc()
    }
  }


  //Compilation OK
  def funcC(cond : Boolean) : Unit@cpsParam[Unit,Unit] = {
    val dummy = if(cond){
      subFunc()
    }
    subFunc()
  }

  //Compilation OK
  def funcD(cond : Boolean) : Unit@cpsParam[Unit,Unit] = {
    if(cond){
      subFunc()
    } else {
      subFunc()
    }
  }

  //Compilation OK
  def funcE(cond : Boolean) : Unit@cpsParam[Unit,Unit] = {
    if(cond){  //This is where the error is pointing
      ???
    }
    subFunc()
  }


  class Dut extends Component {
    val io = new Bundle {
      val a, b, c = in UInt (7 bits)
      val result = out UInt (7 bits)
    }
    val tmp = Bool
    io.result := RegNext(io.a + io.b - io.c)
  }
  def main(args: Array[String]): Unit = {
    def yolo() = 3
    SimConfig.withWave.compile(rtl = new Dut).doSim{ dut =>
      val x = yolo()
      yolo()
      println(dut.io.a.toLong)
      dut.io.a #= 42l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
      dut.io.a #= 54l
      println(dut.io.a.toLong)
      sleep(1)
      println(dut.io.a.toLong)
    }
  }
}
