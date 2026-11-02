package spinal.lib.bus.regif

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink.{Bus, BusParameter, M2sParameters, M2sSupport, M2sTransfers, Opcode, SizeRange}

object TilelinkBusInterface {
  // Single beat Get, PutFullData and PutPartialData only (no bursts, atomics or coherency)
  def getSupported(addressWidth: Int, dataWidth: Int, proposed: M2sSupport): M2sSupport = proposed.copy(
    addressWidth = addressWidth,
    dataWidth = dataWidth,
    transfers = proposed.transfers.intersect(
      M2sTransfers(
        get        = SizeRange.upTo(dataWidth / 8),
        putFull    = SizeRange.upTo(dataWidth / 8),
        putPartial = SizeRange.upTo(dataWidth / 8)
      )
    )
  )

  def getParameter(addressWidth: Int, dataWidth: Int, sourceWidth: Int = 0): BusParameter = M2sParameters(
    support = getSupported(addressWidth, dataWidth, M2sSupport(M2sTransfers.all, addressWidth, dataWidth)),
    sourceCount = 1 << sourceWidth
  ).toNodeParameters().toBusParameter()
}

case class TilelinkBusInterface(bus: Bus, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf {
  assert(!bus.p.withBCE, "TilelinkBusInterface doesn't support the B/C/E channels")
  assert(bus.p.sizeBytes <= bus.p.dataBytes, "TilelinkBusInterface doesn't support bursts, use TilelinkBusInterface.getSupported() for the negotiation")

  override val busDataWidth: Int = bus.p.dataWidth
  override val busAddrWidth: Int = bus.p.addressWidth
  override val withStrb: Boolean = bus.p.withDataA
  override def getModuleName = moduleName.name

  lazy val reg_wrerr: Bool = Reg(Bool(), init = False)
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = Reg(Bool(), init = False)
  val reg_rdata: Bits = Reg(Bits(busDataWidth bits), init = defaultReadBits)

  val wstrb: Bits  = withStrb generate(Bits(strbWidth bit))
  val wmask: Bits  = withStrb generate(Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate(Bits(busDataWidth bit))

  val isGet = Opcode.A.isGet(bus.a.opcode)
  val isPut = Opcode.A.isPut(bus.a.opcode)

  // As with AXI, read side effects (RC, RS, ...) always apply to the whole word. The Get mask is ignored.
  withStrb generate(wstrb := Mux(isGet, B(strbWidth bits, default -> True), bus.a.mask))
  initStrbMasks()

  // reg_rdata, reg_rderr and reg_wrerr are only valid during the cycle after the request got accepted (rsp.first).
  // They are captured so that the response stays stable while D is stalled.
  val rsp = new Area {
    val valid  = RegInit(False)
    val first  = RegInit(False)
    val opcode = Reg(Opcode.D())
    val source = Reg(bus.p.source)
    val size   = Reg(bus.p.size)
    val data   = Reg(Bits(busDataWidth bits))
    val denied = Reg(Bool())
  }

  val halt = False
  val canAccept = !rsp.valid || bus.d.ready

  val askWrite  = (bus.a.valid && isPut).allowPruning()
  val askRead   = (bus.a.valid && isGet).allowPruning()
  val doWrite   = (askWrite && canAccept && !halt).allowPruning()
  val doRead    = (askRead  && canAccept && !halt).allowPruning()
  val writeData = if (bus.p.withDataA) bus.a.data else B(0, busDataWidth bits)

  bus.a.ready := canAccept && !halt

  rsp.first := bus.a.fire
  when (bus.d.ready) {
    rsp.valid := False
  }
  when (bus.a.fire) {
    rsp.valid  := True
    rsp.opcode := isGet.mux(Opcode.D.ACCESS_ACK_DATA(), Opcode.D.ACCESS_ACK())
    rsp.source := bus.a.source
    rsp.size   := bus.a.size
  }
  when (rsp.first) {
    rsp.data   := bus_rdata
    rsp.denied := bus_slverr
  }

  bus.d.valid  := rsp.valid
  bus.d.opcode := rsp.opcode
  bus.d.param  := 0
  bus.d.source := rsp.source
  bus.d.sink   := 0
  bus.d.size   := rsp.size
  bus.d.denied := Mux(rsp.first, bus_slverr, rsp.denied)
  if (bus.p.withDataD) {
    bus.d.data    := Mux(rsp.first, bus_rdata, rsp.data)
    // A denied AccessAckData must also be corrupt
    bus.d.corrupt := bus.d.denied && rsp.opcode === Opcode.D.ACCESS_ACK_DATA
  }

  val address = (bus.a.address.drop(underbitWidth) ## B(0, underbitWidth bit)).asUInt
  override def readAddress()  = address
  override def writeAddress() = address

  override def readHalt()  = halt := True
  override def writeHalt() = halt := True
}
