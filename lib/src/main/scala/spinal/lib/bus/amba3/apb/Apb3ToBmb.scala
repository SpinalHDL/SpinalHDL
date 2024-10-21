package spinal.lib.bus.amba3.apb

import spinal.core._
import spinal.lib.bus._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.bus.bmb.{Axi4SharedToBmb, Bmb, BmbAccessParameter, BmbSourceParameter}
import spinal.lib.{master, slave}

object Apb3ToBmb{
  def getBmbConfig(apb3Config: Apb3Config) = BmbAccessParameter(
    apb3Config.addressWidth, apb3Config.dataWidth
  ).addSources(1, BmbSourceParameter(
    contextWidth = 0,
    lengthWidth = log2Up(apb3Config.dataWidth/4)
  ))
}


class Apb3ToBmb(apb3Config: Apb3Config) extends Component{
  val io = new Bundle{
    val apb = slave(Apb3(apb3Config))
    val bmb = master(Bmb(Apb3ToBmb.getBmbConfig(apb3Config)))
  }

  val cmd = cloneOf(io.bmb.cmd)

  val pending = RegInit(False) setWhen(cmd.fire) clearWhen(io.bmb.rsp.fire)
  io.apb.PREADY := io.bmb.rsp.valid

  cmd.valid := io.apb.PSEL.lsb && io.apb.PENABLE && !pending
  cmd.source := 0
  cmd.opcode := io.apb.PWRITE.asBits
  cmd.address := io.apb.PADDR
  cmd.length := apb3Config.dataWidth/8-1
  cmd.data := io.apb.PWDATA
  cmd.mask.setAll()
  cmd.context := 0
  cmd.last := True
  io.bmb.cmd << cmd.halfPipe()

  io.bmb.rsp.ready := True
  io.apb.PRDATA := io.bmb.rsp.data
  io.apb.PSLVERROR := io.bmb.rsp.isError
}