package spinal.lib.memory.ram.blackbox

import spinal.lib.memory.ram._
import spinal.core._

class ram_1r1w(wrap: Ram1r1w) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName
  this.setDefinitionName(s"mem_1r1w_${mc.depth}x${mc.dw}_${mc.maskName}_${memName}_wrap")

  val io = new Bundle {
    val wclk   = in Bool
    val waddr  = in UInt(mc.aw bits)
    val wdata  = in Bits(mc.dw bits)
    val we_n   = in Bool()
    val bwe_n  = mc.genBWE
    val rclk   = in Bool()
    val raddr  = in UInt(mc.aw bits)
    val re_n   = in Bool()
    val rdata  = out Bits(mc.dw bits)
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)

  // Define io of the VHDL entiry / Verilog module
  // Map the clk
  val cdw = ClockDomain(io.wclk)
  val cdr = ClockDomain(io.rclk)
  cdw.setSyncWith(cdr)
  val caw = new ClockingArea(cdw){
    mem.write(
      address = io.waddr,
      data    = io.wdata,
      enable  = ~io.we_n
    )
  }
  val car = new ClockingArea(cdr){
    io.rdata := mem.readSync(
      address = io.raddr,
      enable = ~io.re_n
    )
  }
//  Remove io_ prefix
  noIoPrefix()
//  addRTLPath("./rtl/RegisterBank.v")                         // Add a verilog file
//  addRTLPath(s"./rtl/myDesign.vhd")                          // Add a vhdl file
//  addRTLPath(s"${sys.env("MY_PROJECT")}/myTopLevel.vhd")     // Use an environement variable MY_PROJECT (System.getenv("MY_PROJECT"))

  def Build() = {
    wrap.clockDomain.setSyncWith(cdw)
    wrap.clockDomain.setSyncWith(cdr)
    this.io.rclk   := wrap.clockDomain.readClockWire
    this.io.wclk   := wrap.clockDomain.readClockWire
    this.io.re_n   := ~wrap.io.rd
    this.io.raddr  := wrap.io.raddr
    wrap.io.rdata  := this.io.rdata

    this.io.we_n   := ~wrap.io.wr

    if(mc.needBWE){
      this.io.bwe_n := ~wrap.io.bwe
    }
    this.io.waddr    := wrap.io.waddr
    this.io.wdata    := wrap.io.wdata
    this
  }
}
