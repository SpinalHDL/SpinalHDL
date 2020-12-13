package spinal.lib.memory.ram.vendors.umc

import spinal.lib.memory.ram._
import spinal.core._
import spinal.lib.slave

class mbb1rw(wrap: Ram1rw) extends MemBlackBox{
  val mc = wrap.mc
  val memName = wrap.memName
  /*Dirty Temporary solution for deadline, TODO*/
  val wrapName = mc.depth match{
    case 256   => s"Mwrapper_rfsp${mc.depth}x${mc.dw}m2b1w1"
    case 512   => s"Mwrapper_rfsp${mc.depth}x${mc.dw}m4b1w1"
    case 10240 => s"Mwrapper_srsp${mc.depth}x${mc.dw}m16b8w1r"
    case 16384 => s"Mwrapper_srsp${mc.depth}x${mc.dw}m16b4w1r"
    case _ => SpinalError("Fix Temporary Solution fo Gnss")
  }

  this.setDefinitionName(wrapName)

  val io = new Bundle { iobd =>
    val CLK    = in Bool
    val A      = in UInt(mc.aw bits)
    val D      = in Bits(mc.dw bits)
    val CEN    = in Bool()
    val WEN    = in Bool()
    val BWEN   = mc.genBWE
    val Q      = out Bits(mc.dw bits)

    val bist = if(mc.withBist) slave(Bist(mc.bistaw, mc.dw)) else null
    val scan = if(mc.withScan) slave(Scan(false, mc.withRepaire)) else null

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

  def Build(): MemBlackBox = {
    wrap.clockDomain.setSyncWith(cd)
    this.io.CLK  := wrap.clockDomain.readClockWire
    this.io.A    := wrap.io.addr
    this.io.CEN  := ~wrap.io.cs
    this.io.WEN  := ~wrap.io.wr
    if(mc.needBWE){
      this.io.BWEN  := ~wrap.io.bwe
    }
    this.io.D  := wrap.io.wdata
    wrap.io.rdata  := this.io.Q

    if(mc.withBist) {wrap.io.bist >> io.bist}
    if(mc.withScan) {wrap.io.scan >> io.scan}
    this
  }
  noIoPrefix()
//  mapClockDomain(clock = io.CLK)

  val mem = new Mem(Bits(mc.dw bits), mc.depth)

  if(mc.withBist){ io.bist.tileZero()}
  if(mc.withScan){ io.scan.tileZero()}

  val ca  = new ClockingArea(cd) {
    mem.write(
      address = io.A,
      data = io.D,
      enable = ~io.WEN
    )

    io.Q := mem.readSync(
      address = io.A,
      enable = (!io.CEN && io.WEN)
    )
  }

  override type RefOwnerType = this.type
}
