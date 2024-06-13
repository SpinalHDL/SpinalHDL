package spinal.lib.bus.localbus.sim

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.localbus.MinBus

case class MinBusDriver(bus : MinBus, clockDomain : ClockDomain) {
  bus.ce #= false
  bus.wr #= false
  bus.addr  #= 0
  bus.wdat  #= 0
  bus.strb  #= 0
  bus.prot  #= 0

  var verbose = false
  def write(address : BigInt, data : BigInt, strb: BigInt = 0xFFFF, prot: Int=0) : Unit = {
    if(verbose) println(s"MinBus[0x${address.toString(16)}] = 0x${data.toString(16)}")
    bus.ce   #= true
    bus.wr   #= true
    bus.addr #= address
    bus.wdat #= data
    bus.strb #= strb%(1<<bus.c.sw)
    bus.prot #= prot
    clockDomain.waitSamplingWhere(bus.rdy.toBoolean)
    bus.ce   #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
    bus.strb.randomize()
    bus.prot.randomize()
  }

  def read(address : BigInt) : BigInt = {
    bus.ce #= true
    bus.wr #= false
    bus.addr #= address
    bus.wdat.randomize()
    bus.strb.randomize()
    bus.prot.randomize()
    clockDomain.waitSamplingWhere(bus.rdy.toBoolean)
    bus.ce #= false
    bus.wr.randomize()
    bus.addr.randomize()
    bus.wdat.randomize()
    bus.strb.randomize()
    bus.prot.randomize()
    clockDomain.waitSamplingWhere(bus.rvld.toBoolean)
    bus.rdat.toBigInt
  }
}