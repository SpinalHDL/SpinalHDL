package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{M2sAgent, M2sParameters, M2sSource, M2sTransfers, NodeParameters, SizeRange}

import scala.collection.mutable.ArrayBuffer

object TilelinkToBmb{
  def getBmbParam(p : tilelink.M2sParameters): BmbAccessParameter = BmbAccessParameter(
    addressWidth = p.addressWidth,
    dataWidth = p.dataWidth,
    sources = p.masters.flatMap(m =>
      m.mapping.flatMap { m2 =>
        var e = ArrayBuffer[(Int, BmbSourceParameter)]()
        m2.id.foreach { id =>
          e += (id.toInt -> BmbSourceParameter(
            contextWidth = log2Up(p.sizeBytes)+1,
            lengthWidth = log2Up(m2.emits.sizeBytes),
            alignment = BmbParameter.BurstAlignement.LENGTH,
            canRead = true,
            canWrite = true,
            canMask = false
          ))
        }
        e
      }
    ).toMapLinked()
  )
}

// Assume BMB memory accesses are power of 2 length and aligned !!
// Will only issue one outstanding transaction at the time (single source)
case class TilelinkToBmb(p : tilelink.M2sParameters) extends Component{
  val io = new Bundle {
    val up = slave(tilelink.Bus(p))
    val down = master(Bmb(TilelinkToBmb.getBmbParam(p)))
  }

  io.down.cmd.arbitrationFrom(io.up.a)
  io.down.cmd.source    := io.up.a.source
  io.down.cmd.opcode(0) := io.up.a.opcode =/= tilelink.Opcode.A.GET
  io.down.cmd.address   := io.up.a.address
  io.down.cmd.length    := ((U(1) << io.up.a.size)-1).resized
  io.down.cmd.data      := io.up.a.data
//  io.down.cmd.mask      := io.up.a.mask
  io.down.cmd.context   := io.up.a.size ## io.down.cmd.opcode

  io.up.d.arbitrationFrom(io.down.rsp)
  io.up.d.opcode  := io.down.rsp.context(0).mux(tilelink.Opcode.D.ACCESS_ACK, tilelink.Opcode.D.ACCESS_ACK_DATA)
  io.up.d.param   := 0
  io.up.d.source  := io.down.rsp.source
  io.up.d.size    := io.down.rsp.context.dropLow(1).asUInt
  io.up.d.denied  := io.down.rsp.isError
  io.up.d.data    := io.down.rsp.data
  io.up.d.corrupt := False
}



