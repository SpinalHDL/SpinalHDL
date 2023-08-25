package spinal.lib.bus.tilelink
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class Ram (p : NodeParameters) extends Component {
  val io = new Bundle{
    val up = slave port Bus(p)
  }

  val mem = Mem.fill((1 << p.m.addressWidth)/p.m.dataBytes)(Bits(p.m.dataWidth bits))
  val write = mem.writePortWithMask(p.m.dataBytes)
  val read = mem.readSyncPort()
  val pipeline = new Pipeline{
    val cmd = new Stage{
      driveFrom(io.up.a)
      val A = insert(io.up.a.payload)

      val address = A.address >> log2Up(p.m.dataBytes)
      write.valid := valid && Opcode.A.isPut(io.up.a.opcode)
      write.address := address
      write.data := A.data
      write.mask := A.mask

      read.cmd.valid := valid && Opcode.A.isGet(io.up.a.opcode)
      read.cmd.payload := address
      ???
    }
    build()
  }
}
