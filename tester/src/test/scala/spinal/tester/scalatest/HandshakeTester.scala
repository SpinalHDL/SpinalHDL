package spinal.tester.scalatest

import spinal.core._
import spinal.lib._


object HandshakeTester{
  case class BundleA(aaa : Int) extends Bundle{
    val a = UInt(8 bit)
    val b = Bool
  }
}

import spinal.tester.scalatest.HandshakeTester._

class HandshakeTester extends Component {
  val io = new Bundle {
    val slave0 = slave Handshake new BundleA(8)
    val master0 = master Handshake new BundleA(8)
    val fifo0_occupancy = out UInt
  }

  val fifo0 = new HandshakeFifo(new BundleA(8),16)
  fifo0.io.push << io.slave0
  fifo0.io.pop >/-> io.master0
  io.fifo0_occupancy := fifo0.io.occupancy

  assert(3 == latencyAnalysis(io.slave0.data.a,fifo0.ram,io.master0.data.a))
  assert(2 == latencyAnalysis(io.master0.ready,io.slave0.ready))
}



class HandshakeTesterBoot extends SpinalTesterBase {
  override def getName: String = "HandshakeTester"
  override def createToplevel: Component = new HandshakeTester
}