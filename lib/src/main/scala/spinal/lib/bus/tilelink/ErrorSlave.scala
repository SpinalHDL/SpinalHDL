package spinal.lib.bus.tilelink

import spinal.core._
import spinal.lib._

class ErrorSlave(bp : BusParameter) extends Component{
  val io = new Bundle{
    val bus = slave(Bus(bp))
  }

  val ad = new Area{
    val buffer = io.bus.a.halfPipe()
    buffer.ready := io.bus.d.fire && io.bus.d.isLast()
    io.bus.d.valid := buffer.valid && buffer.isLast()
    io.bus.d.opcode := buffer.opcode mux(
      Opcode.A.PUT_FULL_DATA    -> Opcode.D.ACCESS_ACK(),
      Opcode.A.PUT_PARTIAL_DATA -> Opcode.D.ACCESS_ACK(),
      Opcode.A.GET              -> Opcode.D.ACCESS_ACK_DATA(),
      Opcode.A.ACQUIRE_BLOCK    -> Opcode.D.GRANT_DATA(),
      Opcode.A.ACQUIRE_PERM     -> Opcode.D.GRANT()
    )
    io.bus.d.param := 0
    io.bus.d.source := buffer.source
    io.bus.d.sink := 0
    io.bus.d.size := buffer.size
    io.bus.d.denied := True
    if(bp.withDataD) {
      io.bus.d.data.assignDontCare()
      io.bus.d.corrupt.assignDontCare()
    }
  }
  assert(!bp.withBCE)
}
