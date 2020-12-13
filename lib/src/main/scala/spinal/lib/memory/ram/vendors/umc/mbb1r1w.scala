package spinal.lib.memory.ram.vendors.umc

import spinal.lib.memory.ram._
import spinal.core._
import spinal.lib.slave

class mbb1r1w(wrap: Ram1r1w) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName

  this.setDefinitionName(s"Mwrapper_rf2pu${mc.depth}x${mc.dw}m4b2w1")

  val io = new Bundle { iobd =>
    val QA     = out Bits(mc.dw bits)
    val CLK    = in Bool
    val CENA   = in Bool()
    val AA     = in UInt(mc.aw bits)
    val CENB   = in Bool()
    val WENB   = in Bool()
    val BWENB  = mc.genBWE
    val AB     = in UInt(mc.aw bits)
    val DB     = in Bits(mc.dw bits)

    val bist   = if(mc.withBist) slave(Bist(mc.bistaw, mc.dw)) else null
    val scan   = if(mc.withScan) slave(Scan(true, mc.withRepaire)) else null

    if(mc.withBist){
      iobd.getClass.getMethod("bist").invoke(iobd).asInstanceOf[Data].setName("")
    }
    if(mc.withScan){
      iobd.getClass.getMethod("scan").invoke(iobd).asInstanceOf[Data].setName("")
      if(scan.repaire){
        scan.rp.setName("")
      }
      if(!scan.short){
        scan.ex.setName("")
      }
    }
  }


  val cd  = ClockDomain(io.CLK)

  def Build() = {
    wrap.clockDomain.setSyncWith(cd)
    this.io.CLK  := wrap.clockDomain.readClockWire
    this.io.CENA := ~wrap.io.rd
    this.io.AA   :=  wrap.io.raddr
    this.io.CENB := ~wrap.io.wr
    this.io.WENB := ~wrap.io.wr
    if(mc.needBWE){
      this.io.BWENB  := ~wrap.io.bwe
    }
    this.io.AB  := wrap.io.waddr
    this.io.DB  := wrap.io.wdata
    wrap.io.rdata  := this.io.QA
    if(mc.withScan){ wrap.io.scan >> this.io.scan}
    if(mc.withBist){ wrap.io.bist >> this.io.bist}
    this
  }

  noIoPrefix()
//  mapClockDomain(clock = io.CLK)

  if(mc.withBist){ io.bist.tileZero()}
  if(mc.withScan){ io.scan.tileZero()}

  val mem = new Mem(Bits(mc.dw bits), mc.depth)

  // Define io of the VHDL entiry / Verilog module
  // Map the clk
  val caw = new ClockingArea(cd){
    mem.write(
      address = io.AB,
      data    = io.DB,
      enable  = ~io.WENB
    )
  }
  val car = new ClockingArea(cd){
    io.QA := mem.readSync(
      address = io.AA,
      enable = ~io.CENA
    )
  }
}
