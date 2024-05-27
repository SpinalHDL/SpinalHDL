package spinal.lib.bus.localbus

import spinal.core._
import spinal.core.sim._

case class MemBusDriver(bus : MemBus, clockDomain : ClockDomain) {
  bus.ce #= false
  bus.wr #= false
  bus.addr  #= 0
  bus.wdat  #= 0

  var verbose = false
  def write(address : BigInt, data : BigInt) : Unit = {
    if(verbose) println(s"Mem[0x${address.toString(16)}] = 0x${data.toString(16)}")
    bus.ce   #= true
    bus.wr   #= true
    bus.addr #= address
    bus.wdat #= data
    clockDomain.waitSampling()
    bus.ce   #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
  }

  def read(address : BigInt) : BigInt = {
    bus.ce #= true
    bus.wr #= false
    bus.addr #= address
    bus.wdat #= 1234 //.randomize()
    clockDomain.waitSampling()
    bus.ce #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
    sleep(0)
    bus.rdat.toBigInt
  }
}