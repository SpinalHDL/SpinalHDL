package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm.{State, StateMachine}

case class Apb3BridgeConfig(apb4 : Boolean = false)

object Apb3Bridge{
  def getApb3Config(p : NodeParameters, c : Apb3BridgeConfig): Apb3Config ={
    assert(!p.withBCE)
    assert(p.m.emits.isOnlyGetPut())
    Apb3Config(
      addressWidth = p.m.addressWidth,
      dataWidth    = p.m.dataWidth,
      useStrb = c.apb4,
      useProt = c.apb4
    )
  }
  def getSupported(proposed : M2sSupport, c : Apb3BridgeConfig) = {
    proposed intersect M2sTransfers(
      get = SizeRange(1, 4096),
      putFull = SizeRange(c.apb4.mux(1, proposed.dataWidth/8), 4096)
    )
  }
}

class Apb3Bridge(p : NodeParameters, c : Apb3BridgeConfig) extends Component{
  val apbConfig = Apb3Bridge.getApb3Config(p, c)
  val io = new Bundle{
    val up = slave port Bus(p)
    val down = master port Apb3(apbConfig)
  }

  val buffered = io.up.a.halfPipe() //Required as we don't want to start io.up.d before consuming io.up.a (GET)
  val isGet = buffered.opcode === Opcode.A.GET
  val counter = Reg(io.up.p.beat) init(0)
  val isLast = counter === buffered.sizeToBeatMinusOne()
  val forked = buffered.forkSerial(!isGet || isLast)
  when(forked.fire) {
    counter := (counter + 1).resized
    when(isLast) {
      counter := 0
    }
  }

  val enable = RegInit(False)
  enable := enable.mux(!io.down.PREADY, io.down.PSEL(0))

  forked.ready := enable && io.down.PREADY
  io.down.PSEL(0) := buffered.valid
  io.down.PENABLE := enable
  io.down.PADDR := buffered.address.clearedLow(log2Up(apbConfig.dataBytes)) | (counter << log2Up(p.m.dataBytes)).resized
  io.down.PWRITE := !isGet
  io.down.PWDATA := buffered.data
  if(apbConfig.useProt) io.down.PPROT := 0
  if(apbConfig.useStrb) io.down.PSTRB := buffered.mask

  val rsp = cloneOf(io.up.d)
  val rspOpcode = isGet.mux(Opcode.D.ACCESS_ACK_DATA, Opcode.D.ACCESS_ACK)
  rsp.valid := forked.fire
  rsp.opcode := rspOpcode
  rsp.param := 0
  rsp.source := buffered.source
  rsp.sink := 0
  rsp.size := buffered.size
  rsp.data := io.down.PRDATA
  rsp.denied := io.down.PSLVERROR
  rsp.corrupt := io.down.PSLVERROR && Opcode.D.isData(rspOpcode)

  io.up.d << rsp.halfPipe()
  io.down.PSEL(0) clearWhen(io.up.d.valid)
}


object Apb3BridgeGen extends App{
  SpinalVerilog(new Apb3Bridge(
    new M2sParameters(
      addressWidth = 32,
      dataWidth = 32,
      masters = List.fill(2)(
        M2sAgent(
          name = null,
          M2sSource(
            id = SizeMapping(0, 16),
            emits = M2sTransfers(
              get = SizeRange.upTo(4),
              putFull = SizeRange.upTo(4)
            )
          )
        )
      )
    ).toNodeParameters(),
    Apb3BridgeConfig()
  ))
}
