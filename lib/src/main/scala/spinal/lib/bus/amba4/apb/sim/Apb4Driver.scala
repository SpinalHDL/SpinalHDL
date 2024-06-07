package spinal.lib.bus.amba4.apb.sim

import spinal.core.ClockDomain
import spinal.core.sim._
import spinal.lib.bus.amba4.apb.Apb4

case class Apb4Driver(apb : Apb4, clockDomain : ClockDomain) {
//  apb.PSEL #= 0
//  apb.PENABLE #= false

  var verbose = false

  def write(address : BigInt, data : BigInt, strb: BigInt = 0xFF, prot: Int=0) : Unit = {
    if(verbose) println(s"APB[0x${address.toString(16)}] = 0x${data.toString(16)}")
    apb.PSEL #= 1
    apb.PENABLE #= false
    apb.PWRITE  #= true
    apb.PADDR   #= address
    apb.PWDATA  #= data
    apb.PPROT   #= prot%8
    apb.PSTRB   #= strb%(1<<apb.c.strbWidth)
    clockDomain.waitSampling()
    apb.PENABLE #= true
    clockDomain.waitSamplingWhere(apb.PREADY.toBoolean)
    apb.PSEL #= 0
    apb.PENABLE.randomize()
    apb.PADDR.randomize()
    apb.PPROT.randomize()
    apb.PSTRB.randomize()
    apb.PWDATA.randomize()
    apb.PWRITE.randomize()
  }

  /*
  * why read use strb?
  * RC/RS/WSRC/WCRS accestype will do operation on reg when read occur
  * There are two options
  * 1. Do Not use strb
  * 2. Use strb to provide better flexibility. If you really don't need it, then you can read with strb high
  * Here we agree that regif chooses to use strb when reading
  * */
  def read(address : BigInt, strb: BigInt = 0xFFFF, prot: Int = 0) : BigInt = {
    apb.PSEL    #= 1
    apb.PENABLE #= false
    apb.PADDR   #= address
    apb.PWRITE  #= false
    apb.PPROT   #= prot%8
    apb.PSTRB   #= strb%(1<<apb.c.strbWidth)
    clockDomain.waitSampling()
    apb.PENABLE #= true
    clockDomain.waitSamplingWhere(apb.PREADY.toBoolean)
    apb.PSEL    #= 0
    apb.PENABLE.randomize()
    apb.PADDR.randomize()
    apb.PWDATA.randomize()
    apb.PPROT.randomize()
    apb.PSTRB.randomize()
    apb.PWRITE.randomize()
    apb.PRDATA.toBigInt
  }
}