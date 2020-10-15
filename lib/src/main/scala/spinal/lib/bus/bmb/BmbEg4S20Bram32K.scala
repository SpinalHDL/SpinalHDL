package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.misc.BinTools
import spinal.lib.blackbox.anlogic.eagle.EG_PHY_BRAM32K

object BmbEg4S20Bram32K{
  def busCapabilities(size : BigInt) = BmbAccessCapabilities(
    addressWidth   = log2Up(size),
    dataWidth      = 32,
    lengthWidthMax = 2,
    alignment      = BmbParameter.BurstAlignement.LENGTH
  )
}

//Provide a list of 16 on-chip-rams of size 4096=2^12 bytes = 2^15 bits each via the Eg4S20 BRAM32K.
case class BmbEg4S20Bram32K(p: BmbParameter,
                            hexInit : String = null) extends Component{
  val io = new Bundle{
    val bus = slave(Bmb(p))
  }

  assert(p.access.addressWidth >= 13)
  val bankSel = io.bus.cmd.address >> 13
  val bankCount = 1 << (p.access.addressWidth - 13)

  val banks = for(bankId <- 0 until bankCount) yield new Area {

    val mems = List.fill(2)(EG_PHY_BRAM32K())
    mems(0).dia := io.bus.cmd.data(15 downto 0)
    mems(1).dia := io.bus.cmd.data(31 downto 16)
    mems(0).bytea := io.bus.cmd.mask(1) | io.bus.cmd.mask(0)
    mems(1).bytea := io.bus.cmd.mask(3) | io.bus.cmd.mask(2)
    if(hexInit != null) {
      mems(0).addGeneric("INIT_FILE", hexInit + "_0_" + bankId + ".dat")
      mems(1).addGeneric("INIT_FILE", hexInit + "_1_" + bankId + ".dat")
    }

    for (mem <- mems) {
      mem.addGeneric("MODE", "SP16K")
      mem.csa := io.bus.cmd.valid && bankSel === bankId
      mem.addra := (io.bus.cmd.address >> 2).resized
      mem.wea := io.bus.cmd.isWrite
      mem.bytewea := False
      mem.ocea := False
      mem.rsta := False
    }
    val readData = mems(1).doa ## mems(0).doa

    for (mem <- mems) {
      mem.dib := 0
      mem.byteb := False
      mem.csb := False
      mem.addrb := 0
      mem.web := False
      mem.byteweb := False
      mem.oceb := False
      mem.rstb := False
      mem.clkb := False
    }
  }

  val rspBankSel = RegNextWhen(io.bus.cmd.address >> 13,  io.bus.cmd.ready)
  io.bus.cmd.ready   := !io.bus.rsp.isStall
  io.bus.rsp.valid   := RegNextWhen(io.bus.cmd.valid,   io.bus.cmd.ready) init(False)
  io.bus.rsp.source  := RegNextWhen(io.bus.cmd.source,  io.bus.cmd.ready)
  io.bus.rsp.context := RegNextWhen(io.bus.cmd.context, io.bus.cmd.ready)
  io.bus.rsp.data  := banks.map(_.readData).read(rspBankSel)
  io.bus.rsp.setSuccess()
  io.bus.rsp.last := True
}
