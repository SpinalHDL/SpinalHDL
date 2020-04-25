package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.{StreamFifoMultiChannel, master, slave}
import spinal.lib.bus.bmb.sim.{BmbInterconnectTester, BmbMasterAgent, BmbMemoryAgent, BmbMemoryTester}
import spinal.lib.bus.bmb.{Bmb, BmbDecoder, BmbDecoderOutOfOrder, BmbParameter}
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random




class SpinalSimBmbDecoderOutOfOrderTester extends FunSuite {
  def doIt(outputCount : Int, sourceCount : Int): Unit ={
    val p = BmbParameter(
      addressWidth = 20,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = log2Up(sourceCount),
      contextWidth = 16
    )
    SimConfig.compile(new BmbDecoderOutOfOrder(
      p             = p,
      mappings      = Seq(DefaultMapping) ++ Seq.tabulate(outputCount)(e => SizeMapping(0x40000*e, 0x40000)),
      capabilities  = Seq.fill(outputCount + 1)(p),
      pendingRspMax = 64
    )).doSimUntilVoid(seed = 42) { dut =>
      SimTimeout(10000000)
      dut.clockDomain.forkStimulus(2)

      val tester = new BmbInterconnectTester()
      tester.perSourceRspCountTarget = 10000
      tester.addMaster(dut.io.input, dut.clockDomain)
      for(portId <- 0 until dut.mappings.size) tester.addSlave(
        bus          = dut.io.outputs(portId),
        mapping      = dut.mappings(portId),
        offset   = 0,
        cd  = dut.clockDomain
      )
    }
  }
  test("2p_4s") {doIt(1, 4)}
  test("4p_4s") {doIt(3, 4)}
  test("2p_8s") {doIt(1, 8)}
  test("4p_8s") {doIt(3, 8)}
}


class SpinalSimBmbDecoderInOrderTester extends FunSuite {
  test("t1") {
    val p = BmbParameter(
      addressWidth = 20,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = 2,
      contextWidth = 16
    )
    SimConfig.compile(BmbDecoder(
      p             = p,
      mappings      = Seq(DefaultMapping, SizeMapping(0x40000, 0x40000)),
      capabilities  = Seq.fill(2)(p),
      pendingMax = 15
    )).doSimUntilVoid(seed = 42) { dut =>
      SimTimeout(1000000)
      dut.clockDomain.forkStimulus(2)

      val tester = new BmbInterconnectTester()
      tester.addMaster(dut.io.input, dut.clockDomain)
      for(portId <- 0 until dut.mappings.size) tester.addSlave(
        bus          = dut.io.outputs(portId),
        mapping      = dut.mappings(portId),
        offset   = 0,
        cd  = dut.clockDomain
      )
    }
  }
}



object SpinalSimBmbDecoderOutOfOrderSynthesisBench extends App{
  val payloadType = HardType(Bits(8 bits))
  class BenchFpga(sourceCount : Int, outputCount : Int) extends Rtl{
    val p = BmbParameter(
      addressWidth = 32,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = log2Up(sourceCount),
      contextWidth = 4
    )
    override def getName(): String = s"Bench_${sourceCount}_$outputCount"
    override def getRtlPath(): String = getName() + ".v"
    SpinalVerilog(new Component{
      val input = slave(Bmb(p))
      val outputs = Vec(master(Bmb(p)), outputCount)
      val decoder = new BmbDecoderOutOfOrder(
        p             = p,
        mappings      = Seq(DefaultMapping) ++ Seq.tabulate(outputCount-1)(e => SizeMapping(0x40000*e, 0x40000)),
        capabilities  = Seq.fill(outputCount)(p),
        pendingRspMax = 64
      )
      var sum = 0
      decoder.io.input.cmd <-/< input.cmd
      decoder.io.input.rsp >/-> input.rsp
      sum += widthOf(input.cmd.payload)
      sum += widthOf(input.rsp.payload)

      (decoder.io.outputs, outputs).zipped.foreach{case (d, o) =>
        o.cmd <-/< d.cmd
        o.rsp >/-> d.rsp
        sum += widthOf(d.cmd.payload)
        sum += widthOf(d.rsp.payload)
      }
      println(sum)

      setDefinitionName(BenchFpga.this.getName())
    })
  }



  val rtls =  List(new BenchFpga(2,2), new BenchFpga(4,2))

  val targets = XilinxStdTargets()// ++ AlteraStdTargets()

//  System.exit(0)

  Bench(rtls, targets)
}