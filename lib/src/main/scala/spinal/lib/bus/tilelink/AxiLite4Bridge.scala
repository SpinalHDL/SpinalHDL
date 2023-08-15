package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping

object AxiLite4Bridge{
  def getAxiLite4Config(p : NodeParameters): AxiLite4Config ={
    assert(!p.withBCE)
    assert(p.m.emits.isOnlyGetPut())
    assert(p.sizeBytes <= p.m.dataBytes)
    AxiLite4Config(
      addressWidth = p.m.addressWidth,
      dataWidth    = p.m.dataWidth
    )
  }
  def getSupported(proposed : M2sSupport) = proposed.intersect(
    M2sTransfers(
      get        = SizeRange.upTo(proposed.dataBytes),
      putFull    = SizeRange.upTo(proposed.dataBytes),
      putPartial = SizeRange.upTo(proposed.dataBytes)
    )
  )
}

class AxiLite4Bridge(p : NodeParameters) extends Component{
  val axiConfig = AxiLite4Bridge.getAxiLite4Config(p)
  val io = new Bundle{
    val up = slave port Bus(p)
    val down = master port AxiLite4(axiConfig)
  }

  val pending = new Area{
    val valid = RegInit(False) clearWhen(io.up.d.fire)
    val get = Reg(Bool())
    val source = Reg(io.up.p.source)
    val size = Reg(io.up.p.size)
    when(io.up.a.fire){
      valid := True
      get := io.up.a.opcode === Opcode.A.GET
      source := io.up.a.source
      size := io.up.a.size
    }
  }

  val a = new Area {
    val halted = io.up.a.haltWhen(pending.valid)

    val (cmdFork, dataFork) = StreamFork2(halted)
    val cmd = new Area {
      val filtred = cmdFork.takeWhen(cmdFork.isFirst())
      val isGet = filtred.opcode === Opcode.A.GET

      io.down.aw.valid := filtred.valid && !isGet
      io.down.ar.valid := filtred.valid &&  isGet
      filtred.ready := isGet.mux(io.down.ar.ready, io.down.aw.ready)

      for (ax <- List(io.down.aw, io.down.ar)) {
        ax.addr := filtred.address
        ax.prot := 0
      }
    }
    val data = new Area{
      val filtred = dataFork.takeWhen(dataFork.opcode === Opcode.A.PUT_FULL_DATA || dataFork.opcode === Opcode.A.PUT_PARTIAL_DATA)
      val buffer = filtred.pipelined(m2s = true)
      io.down.w.arbitrationFrom(buffer)
      io.down.w.data := buffer.data
      io.down.w.strb := buffer.mask
    }
  }

  val d = new Area{
    io.down.r.ready := io.up.d.ready
    io.down.b.ready := io.up.d.ready
    io.up.d.valid := io.down.r.valid || io.down.b.valid
    io.up.d.opcode := pending.get.mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
    io.up.d.param := 0
    io.up.d.source := pending.source
    io.up.d.sink := 0
    io.up.d.denied := !pending.get.mux(io.down.r.isOKAY(), io.down.b.isOKAY())
    io.up.d.data := io.down.r.data
    io.up.d.corrupt := False
    io.up.d.size := pending.size
  }
}


object AxiLite4BridgeGen extends App{
  SpinalVerilog(new AxiLite4Bridge(
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