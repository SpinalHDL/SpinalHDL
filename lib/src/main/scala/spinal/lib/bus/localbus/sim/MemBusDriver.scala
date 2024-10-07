package spinal.lib.bus.localbus

import spinal.core._
import spinal.core.sim._

case class MemBusDriver(bus : MemBus, clockdomain : ClockDomain) {
  var verbose = false
  bus.ce   #= false
  bus.wr   #= false
  bus.addr #= 0
  bus.wdat #= 0

  def write(address : BigInt, data : BigInt) : Unit = {
    if(verbose) println(s"Mem[0x${address.toString(16)}] = 0x${data.toString(16)}")
    bus.ce   #= true
    bus.wr   #= true
    bus.addr #= address
    bus.wdat #= data
    clockdomain.waitSampling()
    bus.ce   #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
  }

  def read(address : BigInt) : BigInt = {
    bus.ce #= true
    bus.wr #= false
    bus.addr #= address
    bus.wdat.randomize()
    clockdomain.waitSampling()
    bus.ce #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
    sleep(0)
    sleep(0)
    bus.rdat.toBigInt
  }

  def simClear(): Unit = {
    bus.ce #= false
    bus.wr #= false
    bus.addr #= 0
    bus.wdat #= 0
  }
  def hangMem(): MemVIP = (new MemVIP).hang(this.bus)(clockdomain)
}