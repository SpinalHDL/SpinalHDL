package spinal.tester.scalatest

import org.scalatest.FunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.{StreamFifoMultiChannelSharedSpace, master, slave}
import spinal.lib.bus.bmb.sim.{BmbInterconnectTester, BmbMasterAgent, BmbMemoryAgent, BmbMemoryTester}
import spinal.lib.bus.bmb.{Bmb, BmbAccessParameter, BmbDecoder, BmbDecoderOutOfOrder, BmbDecoderPerSource, BmbParameter, BmbSourceParameter}
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random




class SpinalSimBmbDecoderOutOfOrderTester extends FunSuite {
  def doIt(outputCount : Int, sourceCount : Int, withDefault : Boolean): Unit ={
    val p = BmbParameter(
      addressWidth = 20,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = log2Up(sourceCount),
      contextWidth = 16
    )
    SimConfig.compile(new BmbDecoderOutOfOrder(
      p             = p,
      mappings      = Seq.tabulate(outputCount)(e => if(e == 0 && withDefault) DefaultMapping else SizeMapping(0x40000*e, 0x40000)),
      capabilities  = Seq.fill(outputCount)(p),
      pendingRspTransactionMax = 64
    )).doSimUntilVoid(seed = 42) { dut =>
      SimTimeout(10000000)
      dut.clockDomain.forkStimulus(2)

      val tester = new BmbInterconnectTester()
      tester.perSourceRspCountTarget = 1000
      tester.addMaster(dut.io.input, dut.clockDomain)
      for(portId <- 0 until dut.mappings.size) tester.addSlave(
        bus          = dut.io.outputs(portId),
        mapping      = dut.mappings(portId),
        offset   = 0,
        cd  = dut.clockDomain
      )
    }
  }
  test("2p_4s") {doIt(2, 4, false)}
  test("4p_4s") {doIt(4, 4, false)}
  test("2p_8s") {doIt(2, 8, false)}
  test("4p_8s") {doIt(4, 8, false)}
  test("2p_4s_wd") {doIt(2, 4, true)}
  test("4p_4s_wd") {doIt(4, 4, true)}
  test("2p_8s_wd") {doIt(2, 8, true)}
  test("4p_8s_wd") {doIt(4, 8, true)}

  test("mixed"){
    val ap = BmbAccessParameter(
      addressWidth = 20,
      dataWidth    = 32
    )
    for(i <- List(1,9,5,6,4)){
//    for(i <- List(0,1,2,3,4,5)){
      ap.sources(i) = BmbSourceParameter(
        lengthWidth  = 6,
        contextWidth = 16
      )
    }
    val p = ap.toBmbParameter()
    val outputCount = 4
    val withDefault = false
    SimConfig.compile(new BmbDecoderOutOfOrder(
      p             = p,
      mappings      = Seq.tabulate(outputCount)(e => if(e == 0 && withDefault) DefaultMapping else SizeMapping(0x40000*e, 0x40000)),
      capabilities  = Seq.fill(outputCount)(p),
      pendingRspTransactionMax = 64
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

class SpinalSimBmbDecoderInOrderPerSourceTester extends FunSuite {
  test("t1") {
    val p = BmbParameter(
      addressWidth = 20,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = 2,
      contextWidth = 16
    )
    SimConfig.compile(BmbDecoderPerSource(
      p             = p,
      mappings      = Seq(DefaultMapping, SizeMapping(0x40000, 0x40000), SizeMapping(0x80000, 0x40000)),
      capabilities  = Seq.fill(3)(p),
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

  test("t2") {
    val p = BmbParameter(
      addressWidth = 20,
      dataWidth    = 32,
      lengthWidth  = 6,
      sourceWidth  = 2,
      contextWidth = 16
    )
    SimConfig.withWave.compile(BmbDecoderPerSource(
      p             = p,
      mappings      = Seq(SizeMapping(0x40000, 0x40000), SizeMapping(0x80000, 0x40000)),
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
        pendingRspTransactionMax = 64
      )
      var sum = 0
      decoder.io.input.cmd << input.cmd
      decoder.io.input.rsp >> input.rsp
      sum += widthOf(input.cmd.payload)
      sum += widthOf(input.rsp.payload)

      (decoder.io.outputs, outputs).zipped.foreach{case (d, o) =>
        o.cmd << d.cmd
        o.rsp >> d.rsp
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