package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.fsm.{State, StateMachine}

object Apb3Bridge{
  def getApb3Config(p : NodeParameters): Apb3Config ={
    assert(!p.withBCE)
    assert(p.m.emits.isOnlyGetPut())
    Apb3Config(
      addressWidth = p.m.addressWidth,
      dataWidth    = p.m.dataWidth
    )
  }
  def getSupported(proposed : M2sSupport) = {
    proposed intersect M2sTransfers(
      get = SizeRange(1, 4096),
      putFull = SizeRange(proposed.dataWidth/8, 4096)
    )
  }
}

class Apb3Bridge(p : NodeParameters) extends Component{
  val axiConfig = Apb3Bridge.getApb3Config(p)
  val io = new Bundle{
    val up = slave port Bus(p)
    val down = master port Apb3(axiConfig)
  }

  val buffered = io.up.a.halfPipe() //Required as we don't want to start io.up.d before consuming io.up.a (GET)
  val isGet = buffered.opcode === Opcode.A.GET
  val counter = Reg(io.up.p.beat) init(0)
  val forked = buffered.forkSerial(!isGet || counter === buffered.sizeToBeatMinusOne())
  when(forked.fire) {
    counter := (counter + 1).resized
    when(buffered.fire) {
      counter := 0
    }
  }

  val enable = RegInit(False)
  enable := enable.mux(!io.down.PREADY, io.down.PSEL(0))

  forked.ready := enable && io.down.PREADY
  io.down.PSEL(0) := buffered.valid
  io.down.PENABLE := enable
  io.down.PADDR := buffered.address | (counter << log2Up(p.m.dataBytes)).resized
  io.down.PWRITE := !isGet
  io.down.PWDATA := buffered.data

  val rsp = cloneOf(io.up.d)
  rsp.valid := forked.fire
  rsp.opcode := isGet.mux(Opcode.D.ACCESS_ACK_DATA, Opcode.D.ACCESS_ACK)
  rsp.param := 0
  rsp.source := buffered.source
  rsp.sink := 0
  rsp.size := buffered.size
  rsp.data := io.down.PRDATA
  rsp.denied := io.down.PSLVERROR
  rsp.corrupt := False

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
    ).toNodeParameters()
  ))
}