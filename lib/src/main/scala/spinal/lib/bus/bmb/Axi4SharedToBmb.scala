package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.bmb._

object Axi4SharedToBmb{
  def getBmbConfig(axi4Config: Axi4Config) = BmbAccessParameter(
    axi4Config.addressWidth, axi4Config.dataWidth
  ).addSources((1 << axi4Config.idWidth) << 1, BmbSourceParameter(
    contextWidth = 0,
    lengthWidth = 8 + log2Up(axi4Config.bytePerWord)
  ))
}

//Assume word aligned memory accesses
class Axi4SharedToBmb(axi4Config: Axi4Config) extends Component{
  val io = new Bundle{
    val axi = slave(Axi4Shared(axi4Config))
    val bmb = master(Bmb(Axi4SharedToBmb.getBmbConfig(axi4Config)))
  }

  val hazard = io.axi.arw.write && !io.axi.w.valid
  io.bmb.cmd.valid := io.axi.arw.valid && !hazard
  io.bmb.cmd.source := U(io.axi.arw.write ## io.axi.arw.id)
  io.bmb.cmd.opcode := io.axi.arw.write.asBits
  io.bmb.cmd.address := io.axi.arw.addr
  io.bmb.cmd.length := (io.axi.arw.len << log2Up(axi4Config.bytePerWord)) | (axi4Config.bytePerWord-1)
  io.bmb.cmd.data := io.axi.w.data
  io.bmb.cmd.mask := io.axi.w.strb
  io.bmb.cmd.context := 0
  io.bmb.cmd.last := !io.axi.arw.write || io.axi.w.last
  io.axi.arw.ready := io.bmb.cmd.fire && io.bmb.cmd.last
  io.axi.w.ready := io.bmb.cmd.fire && io.bmb.cmd.isWrite


  val rspIsWrite = io.bmb.rsp.source.msb
  io.axi.b.valid := io.bmb.rsp.valid && rspIsWrite
  io.axi.b.id := io.bmb.rsp.source.resized
  io.axi.b.setOKAY()
  when(io.bmb.rsp.isError) {io.axi.b.setDECERR() }

  io.axi.r.valid := io.bmb.rsp.valid && !rspIsWrite
  io.axi.r.data := io.bmb.rsp.data
  io.axi.r.id := io.bmb.rsp.source.resized
  io.axi.r.last := io.bmb.rsp.last
  io.axi.r.setOKAY()
  when(io.bmb.rsp.isError) {io.axi.r.setDECERR() }

  io.bmb.rsp.ready := (rspIsWrite ? io.axi.b.ready | io.axi.r.ready)
}
