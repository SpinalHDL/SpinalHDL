package spinal.lib.bus.tilelink
import spinal.core._
import spinal.core.sim.SimMemPimper
import spinal.lib._
import spinal.lib.bus.tilelink.coherent.OrderingCmd
import spinal.lib.misc.pipeline._

class Ram (p : NodeParameters,
           bytes : Int) extends Component {
  val io = new Bundle {
    val up = slave port Bus(p)
  }

  val mem = Mem.fill(bytes/p.m.dataBytes)(Bits(p.m.dataWidth bits))
  val port = mem.readWriteSyncPort(p.m.dataBytes)

  val pipeline = new StageCtrlPipeline {
    val cmdNode = ctrl(0)
    val cmd = new cmdNode.Area {
      val IS_GET = insert(Opcode.A.isGet(io.up.a.opcode))
      val SIZE = insert(io.up.a.size)
      val SOURCE = insert(io.up.a.source)
      val LAST = insert(True)

      up.valid := io.up.a.valid
      io.up.a.ready := up.isReady

      val addressShifted = (io.up.a.address >> log2Up(p.m.dataBytes))
      port.enable := up.isFiring
      port.write := !IS_GET
      port.wdata := io.up.a.data
      port.mask := io.up.a.mask

      val withFsm = io.up.p.beatMax != 1
      if (!withFsm) port.address := addressShifted
      val fsm = withFsm generate new Area {
        val counter = Reg(io.up.p.beat) init (0)
        val address = Reg(mem.addressType)
        val size = Reg(io.up.p.size)
        val source = Reg(io.up.p.source)
        val isGet = Reg(Bool())
        val busy = counter =/= 0
        when(busy && isGet) {
          io.up.a.ready := False
          up.valid := True
        }

        when(io.up.a.fire && !busy){
          size := io.up.a.size
          source := io.up.a.source
          isGet := Opcode.A.isGet(io.up.a.opcode)
          address := addressShifted
        }

        LAST clearWhen(counter =/= sizeToBeatMinusOne(io.up.p, SIZE))
        when(busy){
          SIZE := size
          SOURCE := source
          IS_GET := isGet
        }
        when(up.isFiring) {
          counter := counter + 1
          when(LAST) {
            counter := 0
          }
        }
        port.address := busy.mux(address, addressShifted) | counter.resized
      }

    }

    val rspNode = ctrl(1)
    val rsp = new rspNode.Area {
      val takeIt = cmd.LAST || cmd.IS_GET
      haltWhen(!io.up.d.ready && takeIt)
      io.up.d.valid := up.valid && takeIt
      io.up.d.opcode := cmd.IS_GET.mux(Opcode.D.ACCESS_ACK_DATA, Opcode.D.ACCESS_ACK)
      io.up.d.param := 0
      io.up.d.source := cmd.SOURCE
      io.up.d.size := cmd.SIZE
      io.up.d.denied := False
      io.up.d.corrupt := False
      io.up.d.data := port.rdata
    }
    build()
  }

  val ordering = Flow(OrderingCmd(p.sizeBytes))
  ordering.valid := io.up.a.fire && io.up.a.isLast()
  ordering.debugId := io.up.a.debugId
  ordering.bytes := (U(1) << io.up.a.size).resized
  Component.current.addTag(new OrderingTag(ordering.stage()))
}
