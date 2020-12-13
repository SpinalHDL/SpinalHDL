package spinal.lib.memory.ram.vendors.dji

import spinal.lib.memory.ram._
import spinal.core._

class mbb2rw(wrap:  Ram2rw) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName

  this.setDefinitionName(s"mem_1rw_${mc.depth}x${mc.dw}_${mc.maskName}_${memName}_wrap")

  val io = new Bundle {
    val clk_a    = in Bool
    val addr_a   = in UInt(mc.aw bits)
    val wdata_a  = in Bits(mc.dw bits)
    val ce_n_a   = in Bool()
    val we_n_a   = in Bool()
    val bwe_n_a  = mc.genBWE
    val rdata_a  = out Bits(mc.dw bits)

    val clk_b    = in Bool
    val addr_b   = in UInt(mc.aw bits)
    val wdata_b  = in Bits(mc.dw bits)
    val ce_n_b   = in Bool()
    val we_n_b   = in Bool()
    val bwe_n_b  = mc.genBWE
    val rdata_b  = out Bits(mc.dw bits)

    val ctrl_bus = flyWireFromTop(Bits(128 bits), "ctrl_bus")
  }
  noIoPrefix()

  val cda = ClockDomain(io.clk_a)
  val cdb = ClockDomain(io.clk_b)
  cda.setSyncWith(cdb)

  def Build() = {
    wrap.clockDomain.setSyncWith(cda)
    wrap.clockDomain.setSyncWith(cdb)
    this.io.clk_a  := wrap.clockDomain.readClockWire
    this.io.clk_b  := wrap.clockDomain.readClockWire

    this.io.ce_n_a := ~wrap.io.Acs
    this.io.addr_a := wrap.io.Aaddr
    this.io.we_n_a := ~wrap.io.Awr
    wrap.io.Ardata := this.io.rdata_a

    this.io.ce_n_b := ~wrap.io.Bcs
    this.io.addr_b := wrap.io.Baddr
    this.io.we_n_b := ~wrap.io.Bwr
    wrap.io.Brdata := this.io.rdata_b

    if(mc.needBWE){
      this.io.bwe_n_a := ~wrap.io.Abwe
      this.io.bwe_n_b := ~wrap.io.Bbwe
    }
    this
  }

  val mem = new Mem(Bits(mc.dw bits), mc.depth)
  // Define io of the VHDL entiry / Verilog module
  // Map the clk
  val caa = new ClockingArea(cda){
    mem.write(
      address = io.addr_a,
      data    = io.wdata_a,
      enable  = ~io.we_n_a
    )

    io.rdata_a := mem.readSync(
      address = io.addr_a,
      enable  = (!io.ce_n_a && io.we_n_a)
    )
  }
  val cab = new ClockingArea(cdb){
    mem.write(
      address = io.addr_b,
      data    = io.wdata_b,
      enable  = ~io.we_n_b
    )

    io.rdata_a := mem.readSync(
      address = io.addr_b,
      enable  = (!io.ce_n_b && io.we_n_b)
    )
  }
  noIoPrefix()
}

