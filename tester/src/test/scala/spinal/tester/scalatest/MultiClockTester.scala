package spinal.tester.scalatest

import spinal.core._
import spinal.lib._


object MultiClockTester{
  class BundleA extends Bundle{
    val a = UInt(8 bit)
    val b = Bool
  }
}

import spinal.tester.scalatest.HandshakeTester._

class MultiClockTester extends Component {
  import MultiClockTester._
  val io = new Bundle {
    val clkA = in Bool
    val resetA = in Bool
    val clkB = in Bool
    val resetB = in Bool

    val slave0 = slave Handshake(new BundleA)
    val master0 = master Handshake(new BundleA)
    val fifo0_pushOccupancy = out UInt
    val fifo0_popOccupancy = out UInt
  }

  val clockDomainA = ClockDomain(io.clkA,io.resetA)
  val clockDomainB = ClockDomain(io.clkB,io.resetB)

  val fifo0 = new HandshakeFifoCC(new BundleA,16,clockDomainA,clockDomainB)
  fifo0.io.push << io.slave0
  fifo0.io.pop >> io.master0
  io.fifo0_pushOccupancy := fifo0.io.pushOccupancy
  io.fifo0_popOccupancy := fifo0.io.popOccupancy

}



class MultiClockTesterBoot extends SpinalTesterBase {
  override def getName: String = "MultiClockTester"
  override def createToplevel: Component = new MultiClockTester
}