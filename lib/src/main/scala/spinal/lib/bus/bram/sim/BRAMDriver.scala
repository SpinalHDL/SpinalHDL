package spinal.lib.bus.bram.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.bram.BRAM

case class BRAMDriver(bram: BRAM, clockDomain: ClockDomain) {
  bram.en #= false

  var verbose = false

  def write(address: BigInt, data: BigInt, strb: BigInt = 0xffff): Unit = {
    if (verbose) println(s"BRAM[0x${address.toString(16)}] = 0x${data.toString(16)}")
    bram.we #= strb % (1 << bram.we.getWidth)
    bram.addr #= address
    bram.wrdata #= data
    bram.en #= true
    clockDomain.waitSampling()
    bram.en #= false
    bram.we.randomize()
    bram.addr.randomize()
    bram.wrdata.randomize()
  }

  def read(address: BigInt): BigInt = {
    bram.addr #= address
    bram.we #= 0
    bram.en #= true
    clockDomain.waitSampling()
    bram.en #= false
    bram.we.randomize()
    bram.addr.randomize()
    bram.wrdata.randomize()
    clockDomain.waitSampling(bram.config.readLatency)
    bram.rddata.toBigInt
  }
}
