package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.lattice.ice40.SB_SPRAM256KA



object BmbIce40Spram{
  def busCapabilities(size : BigInt) = BmbAccessCapabilities(
    addressWidth   = log2Up(size),
    dataWidth      = 32,
    lengthWidthMax = 2,
    alignment      = BmbParameter.BurstAlignement.LENGTH
  )
}


//Provide a 64 KB on-chip-ram via the Up5k SPRAM.
case class BmbIce40Spram(p: BmbParameter) extends Component{
  val io = new Bundle{
    val bus = slave(Bmb(p))
  }

  assert(p.access.addressWidth >= 16)
  val bankSel = io.bus.cmd.address >> 16
  val bankCount = 1 << (p.access.addressWidth - 16)
  val banks = for(bankId <- 0 until bankCount) yield new Area {
    val mems = List.fill(2)(SB_SPRAM256KA())
    mems(0).DATAIN := io.bus.cmd.data(15 downto 0)
    mems(0).MASKWREN := io.bus.cmd.mask(1) ## io.bus.cmd.mask(1) ## io.bus.cmd.mask(0) ## io.bus.cmd.mask(0)
    mems(1).DATAIN := io.bus.cmd.data(31 downto 16)
    mems(1).MASKWREN := io.bus.cmd.mask(3) ## io.bus.cmd.mask(3) ## io.bus.cmd.mask(2) ## io.bus.cmd.mask(2)
    for (mem <- mems) {
      mem.CHIPSELECT := io.bus.cmd.valid && bankSel === bankId
      mem.ADDRESS := (io.bus.cmd.address >> 2).resized
      mem.WREN := io.bus.cmd.isWrite
      mem.STANDBY := False
      mem.SLEEP := False
      mem.POWEROFF := True
    }

    val readData = mems(1).DATAOUT ## mems(0).DATAOUT
  }

  val rspBankSel = RegNextWhen(io.bus.cmd.address >> 16,  io.bus.cmd.ready)
  io.bus.cmd.ready   := !io.bus.rsp.isStall
  io.bus.rsp.valid   := RegNextWhen(io.bus.cmd.valid,   io.bus.cmd.ready) init(False)
  io.bus.rsp.source  := RegNextWhen(io.bus.cmd.source,  io.bus.cmd.ready)
  io.bus.rsp.context := RegNextWhen(io.bus.cmd.context, io.bus.cmd.ready)
  io.bus.rsp.data  := banks.map(_.readData).read(rspBankSel)
  io.bus.rsp.setSuccess()
  io.bus.rsp.last := True
}
