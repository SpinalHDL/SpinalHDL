package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

class Axi4WriteOnlyOnePerId(config: Axi4Config) extends Component {
  val io = new Bundle {
    val up = slave port Axi4WriteOnly(config)
    val down = master port Axi4WriteOnly(config)
  }

  val pendings = new Area{
    val valids = Reg(Bits(1 << config.idWidth bits)) init(0)
  }

  val onAw = new Area{
    val busy = pendings.valids(io.down.aw.id)
    val halted = io.up.aw.haltWhen(busy)
    when(halted.fire){
      pendings.valids(io.up.aw.id) := True
    }

    val (downFork, wFork) = StreamFork2(halted)
    io.down.aw << downFork

    val toW = Stream(NoData)
    toW.arbitrationFrom(wFork)
  }

  val onW = new Area{
    val aw = onAw.toW.pipelined(m2s = true, s2m = true)
    val w = io.up.w

    val join = StreamJoin(aw.forkSerial(w.last), w)
    io.down.w.arbitrationFrom(join)
    io.down.w.payload := w.payload
  }

  val onB = new Area {
    io.up.b << io.down.b
    when(io.down.b.fire){
      pendings.valids(io.down.b.id) := False
    }
  }
}


object Axi4WriteOnlyOnePerIdGen extends App{
  SpinalVerilog(new Axi4WriteOnlyOnePerId(
    Axi4Config(16,32, 4)
  ))
}