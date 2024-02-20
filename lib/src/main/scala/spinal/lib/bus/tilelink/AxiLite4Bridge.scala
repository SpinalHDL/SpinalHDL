package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.bus.misc.SizeMapping

object AxiLite4Bridge{
  def getAxiLite4Config(p : NodeParameters): AxiLite4Config ={
    assert(!p.withBCE)
    assert(p.m.emits.isOnlyGetPut())
    AxiLite4Config(
      addressWidth = p.m.addressWidth,
      dataWidth    = p.m.dataWidth
    )
  }
  def getSupported(proposed : M2sSupport) = proposed.intersect(M2sTransfers.allGetPut)
}

class AxiLite4Bridge(p : NodeParameters) extends Component{
  val axiConfig = AxiLite4Bridge.getAxiLite4Config(p)
  val io = new Bundle{
    val up = slave port Bus(p)
    val down = master port AxiLite4(axiConfig)
  }


  val pending = new Area {
    val valid = RegInit(False) clearWhen (io.up.d.fire && io.up.d.isLast)
    val get = Reg(Bool())
    val source = Reg(io.up.p.source)
    val size = Reg(io.up.p.size)
  }

  val a = new Area {
    val halted = io.up.a.haltWhen(pending.valid && io.up.a.isFirst)
    when(halted.fire) {
      pending.valid := True
      pending.get := halted.opcode === Opcode.A.GET
      pending.source := halted.source
      pending.size := halted.size
    }

    val buffered = halted.halfPipe()
    val (cmdFork, dataFork) = StreamFork2(buffered)
    val cmd = new Area {
      val isGet = cmdFork.opcode === Opcode.A.GET
      val counter = Reg(io.up.p.beat) init(0)
      val forked = cmdFork.forkSerial(!isGet || counter === cmdFork.sizeToBeatMinusOne())
      when(forked.fire) {
        counter := counter + 1
        when(cmdFork.fire) {
          counter := 0
        }
      }

      io.down.aw.valid := forked.valid && !isGet
      io.down.ar.valid := forked.valid &&  isGet
      forked.ready := isGet.mux(io.down.ar.ready, io.down.aw.ready)

      val address = forked.address | (counter << log2Up(p.m.dataBytes)).resized

      for (ax <- List(io.down.aw, io.down.ar)) {
        ax.addr := address
        ax.prot := 2
      }
    }
    val data = new Area{
      val filtred = dataFork.takeWhen(dataFork.opcode === Opcode.A.PUT_FULL_DATA || dataFork.opcode === Opcode.A.PUT_PARTIAL_DATA)
      io.down.w.arbitrationFrom(filtred)
      io.down.w.data := filtred.data
      io.down.w.strb := filtred.mask
    }
  }

  val d = new Area{
    val counter = Reg(io.up.p.beat) init(0)
    val lastB = counter === io.up.d.sizeToBeatMinusOne()
    when(io.down.b.fire){
      counter := counter + 1
      when(lastB){
        counter := 0
      }
    }

    io.down.r.ready := io.up.d.ready
    io.down.b.ready := io.up.d.ready || !lastB
    io.up.d.valid := io.down.r.valid || io.down.b.valid && lastB
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