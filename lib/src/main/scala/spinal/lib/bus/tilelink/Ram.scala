package spinal.lib.bus.tilelink
import spinal.core._
import spinal.lib._
import spinal.lib.pipeline._

class Ram (p : NodeParameters) extends Component {
  val io = new Bundle{
    val up = slave port Bus(p)
  }

  val mem = Mem.fill((1 << p.m.addressWidth)/p.m.dataBytes)(Bits(p.m.dataWidth bits))
  val port = mem.readWriteSyncPort(p.m.dataBytes)

  val pipeline = new Pipeline{
    val cmd = new Stage{
      val counter = Reg(io.up.p.beat) init(0)
      val LAST = insert(counter === io.up.a.sizeToBeatMinusOne())
      val IS_GET = insert(io.up.a.opcode === Opcode.A.GET)
      val A = insert(io.up.a.payload)

      valid := io.up.a.valid
      io.up.a.ready := isReady && LAST
      when(isFireing){
        counter := counter + 1
        when(LAST){
          counter := 0
        }
      }

      val address = (A.address >> log2Up(p.m.dataBytes)) | counter.resized
      port.enable := isFireing && Opcode.A.isPut(io.up.a.opcode)
      port.address := address
      port.wdata := A.data
      port.mask := A.mask
    }

    val rsp = new Stage(Connection.M2S()){
      val takeIt = cmd.LAST || cmd.IS_GET
      haltWhen(!io.up.d.ready && takeIt)
      io.up.d.valid := valid && takeIt
      io.up.d.opcode := cmd.IS_GET.mux(Opcode.D.ACCESS_ACK_DATA, Opcode.D.ACCESS_ACK)
      io.up.d.param := cmd.A.param
      io.up.d.source := cmd.A.source
      io.up.d.size := cmd.A.size
      io.up.d.denied := False
      io.up.d.corrupt := False
      io.up.d.data := port.rdata
    }
    build()
  }
}
