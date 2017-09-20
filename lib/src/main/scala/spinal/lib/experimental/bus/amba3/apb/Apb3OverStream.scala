package spinal.lib.experimental.bus.amba3.apb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config}

object Apb3OverStream {
  case class ApbCmd(config : Apb3Config) extends Bundle{
    val PADDR      = UInt(config.addressWidth bits)
    val PWRITE     = Bool
    val PWDATA     = Bits(config.dataWidth bits)
  }

  //(cmd,rsp)
  def serialize(apb : Apb3, cmdWidth : Int, rspWidth : Int) = new Area {
    val cmdApb = Stream(ApbCmd(apb.config))
    cmdApb.valid  := apb.PSEL.lsb && apb.PENABLE
    cmdApb.payload.assignSomeByName(apb)
    val cmd : Stream[Bits] = StreamWidthAdapter.make(cmdApb, Bits(cmdWidth bits), padding = true)

    val rsp = Flow(Bits(rspWidth bits))
    val rspApb = StreamWidthAdapter.make(rsp.toStream,apb.PRDATA, padding = true).freeRun()
    apb.PREADY := rspApb.valid
    apb.PRDATA := rspApb.payload

    val cmdSent = RegInit(False) setWhen(cmdApb.fire) clearWhen(apb.PREADY)
    cmdApb.valid clearWhen(cmdSent)
  }


  //(apb)
  def deserialize(cmd : Flow[Bits], rsp : Stream[Bits], apbConfig : Apb3Config) = new Area {
    val apb = Apb3(apbConfig)
    val cmdApb = StreamWidthAdapter.make(cmd.toStream, ApbCmd(apbConfig), padding = true).stage()

    apb.assignSomeByName(cmdApb.payload)
    val state = Counter(2)
    apb.PSEL.lsb := cmdApb.valid
    apb.PENABLE := cmdApb.valid && state === 1
    cmdApb.ready := apb.PREADY && state === 1
    when(cmdApb.valid && !(state === 1 && !apb.PREADY)) {state.increment()}

    val apbRspUnbuffered = Stream(apb.PRDATA)
    apbRspUnbuffered.valid := state === 1 && apb.PREADY
    apbRspUnbuffered.payload := apb.PRDATA
    StreamWidthAdapter(apbRspUnbuffered.stage(), rsp, padding = true)
  }
}
