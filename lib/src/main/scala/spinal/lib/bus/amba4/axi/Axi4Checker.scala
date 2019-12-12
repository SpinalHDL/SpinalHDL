package spinal.lib.bus.amba4.axi

import spinal.core._

case class Axi4SharedChecker(config : Axi4Config, counterWidth : Int = 12) extends Component {
  val io = new Bundle {
    val axi = in(Axi4Shared(config))
    val checks = new Bundle {
      val wBeat = out Bool()
      val rBeat = out Bool()
      val bPending = out Bool()
      val rPending = out Bool()
      val busy = out Bool()
    }
  }

  val bPendings, rPendings, rBeats, wBeats = Reg(UInt(counterWidth bits)) init (0)
  bPendings := bPendings + U(io.axi.arw.fire &&  io.axi.arw.write) - U(io.axi.b.fire)
  rPendings := rPendings + U(io.axi.arw.fire && !io.axi.arw.write) - U(io.axi.r.fire && io.axi.r.last)
  rBeats := rBeats + ((io.axi.arw.fire && !io.axi.arw.write) ? (io.axi.arw.len + U(1, counterWidth bits)) | U(0)) - U(io.axi.r.fire)
  wBeats := wBeats + ((io.axi.arw.fire && io.axi.arw.write) ? (io.axi.arw.len + U(1, counterWidth bits)) | U(0)) - U(io.axi.w.fire)

  io.checks.bPending := bPendings =/= 0
  io.checks.rPending := rPendings =/= 0
  io.checks.rBeat := rBeats =/= 0
  io.checks.wBeat := wBeats =/= 0
  io.checks.busy := io.checks.wBeat || io.checks.rBeat || io.checks.bPending || io.checks.rPending
}


case class Axi4ReadOnlyChecker(config : Axi4Config, counterWidth : Int = 12) extends Component {
  val io = new Bundle {
    val axi = in(Axi4ReadOnly(config))
    val checks = new Bundle {
      val rBeat = out Bool()
      val rPending = out Bool()
      val busy = out Bool()
    }
  }

  val bPendings, rPendings, rBeats, wBeats = Reg(UInt(counterWidth bits)) init (0)
  rPendings := rPendings + U(io.axi.ar.fire) - U(io.axi.r.fire && io.axi.r.last)
  rBeats := rBeats + ((io.axi.ar.fire) ? (io.axi.ar.len + U(1, counterWidth bits)) | U(0)) - U(io.axi.r.fire)

  io.checks.rPending := rPendings =/= 0
  io.checks.rBeat := rBeats =/= 0
  io.checks.busy :=  io.checks.rBeat || io.checks.rPending
}
