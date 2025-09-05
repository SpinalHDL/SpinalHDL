package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{M2sAgent, M2sParameters, M2sSource, M2sTransfers, NodeParameters, SizeRange}

object BmbToTilelink{
  def getTilelinkM2s(p : BmbAccessParameter, name : Nameable): M2sParameters = {
    assert(p.alignment == BmbParameter.BurstAlignement.LENGTH)
    M2sParameters(
      addressWidth = p.addressWidth,
      dataWidth = p.dataWidth,
      masters = List(
        M2sAgent(
          name = name,
          mapping = M2sSource(
            id = SizeMapping(0, 1),
            emits = M2sTransfers(
              get = SizeRange(1, 1 << p.lengthWidth),
              putFull = SizeRange(1, 1 << p.lengthWidth),
              putPartial = SizeRange(1, 1 << p.lengthWidth)
            )
          )
        )
      )
    )
  }
}

// Assume BMB memory accesses are power of 2 length and aligned !!
// Will only issue one outstanding transaction at the time (single source)
case class BmbToTilelink(p : BmbParameter) extends Component{
  val io = new Bundle {
    val up = slave(Bmb(p))
    val down = master(tilelink.Bus(BmbToTilelink.getTilelinkM2s(p.access, BmbToTilelink.this)))
  }

  val pending = RegInit(False) setWhen(io.up.cmd.lastFire) clearWhen(io.up.rsp.lastFire)
  val halted = io.up.cmd.haltWhen(pending)

  io.down.a.arbitrationFrom(halted)
  io.down.a.opcode  := halted.isWrite.mux(tilelink.Opcode.A.PUT_PARTIAL_DATA, tilelink.Opcode.A.GET)
  io.down.a.param   := 0
  io.down.a.source  := 0
  io.down.a.address := halted.address
  io.down.a.mask    := halted.mask
  io.down.a.data    := halted.data
  io.down.a.corrupt := False
  io.down.a.size    := halted.length.muxListDc(
    for(i <- 0 to p.access.lengthWidth; length = (1 << i)-1) yield
      length -> U(i, io.down.a.p.sizeWidth bits)
  )

  io.up.rsp.arbitrationFrom(io.down.d)
  io.up.rsp.opcode(0)  := io.down.d.denied
  io.up.rsp.data       := io.down.d.data
  io.up.rsp.last       := io.down.d.isLast()
}



