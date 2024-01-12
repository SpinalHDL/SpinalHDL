package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink._
import spinal.lib.bus.tilelink


/*
- Ensure ordering (ex : Limit to 1 burst per id)
- Split into aligned chunks
- convert to tilelink
 */

object Axi4WriteOnlyToTilelink{
  def getTilelinkProposal(config: Axi4Config, bytesMax : Int) = {
    val range = SizeRange.upTo(bytesMax)
    M2sSupport(
      addressWidth = config.addressWidth,
      dataWidth = config.dataWidth,
      transfers = M2sTransfers(
        putFull = if(config.useAllStrb) range else SizeRange.none,
        putPartial = range
      )
    )
  }
}

object Axi4ReadOnlyToTilelink{
  def getTilelinkProposal(config: Axi4Config, bytesMax : Int) = {
    val range = SizeRange.upTo(bytesMax)
    M2sSupport(
      addressWidth = config.addressWidth,
      dataWidth = config.dataWidth,
      transfers = M2sTransfers(
        get = range
      )
    )
  }
}


object Axi4ToTilelink{
  def getTilelinkProposal(config: Axi4Config, bytesMax : Int) = {
    val range = SizeRange.upTo(bytesMax)
    M2sSupport(
      addressWidth = config.addressWidth,
      dataWidth = config.dataWidth,
      transfers = M2sTransfers(
        get = range,
        putFull = if (config.useAllStrb) range else SizeRange.none,
        putPartial = range
      )
    )
  }
}


//Assume burst aligned and not more than 1 burst per id inflight
class Axi4WriteOnlyToTilelink(config: Axi4Config, bytesMax : Int) extends Component{
  val dp = Axi4WriteOnlyToTilelink.getTilelinkProposal(config, bytesMax)
  val io = new Bundle {
    val up = slave port Axi4WriteOnly(config)
    val down = master port tilelink.Bus(M2sParameters(dp, 1 << config.idWidth))
  }

  val a = new Area {
    val valid = io.up.aw.valid && io.up.w.valid
    val ok = valid && io.down.a.ready
    io.up.aw.ready := ok && io.up.w.last
    io.up.w.ready := ok
    io.down.a.valid := valid

    val counterRange = log2Up(bytesMax)-1 downto log2Up(config.bytePerWord)
    val counter = Reg(io.down.p.beat) init(0)
    when(io.up.w.fire){
      counter := counter + 1
      when(io.up.w.last){
        counter := 0
      }
    }

    val lenToSize = OHToUInt(OHMasking.last(io.up.aw.len) ## False)
    io.down.a.opcode := tilelink.Opcode.A.PUT_PARTIAL_DATA
    if (config.useAllStrb) when(io.up.aw.allStrb) {
      io.down.a.opcode := tilelink.Opcode.A.PUT_FULL_DATA
    }
    io.down.a.param := 0
    io.down.a.source := io.up.aw.id
    io.down.a.address := io.up.aw.addr
    io.down.a.address(counterRange) := io.up.aw.addr(counterRange) + counter
    io.down.a.size := (lenToSize + io.up.aw.size).resized
    io.down.a.mask := io.up.w.strb
    io.down.a.data := io.up.w.data
    io.down.a.corrupt := False
    io.down.a.debugId := 0
  }

  val d = new Area{
    io.up.b.arbitrationFrom(io.down.d)
    io.up.b.id := io.down.d.source
    io.up.b.setOKAY()
    when(io.down.d.denied){io.up.b.setSLVERR()}
  }
}

//Assume burst aligned and not more than 1 burst per id inflight
class Axi4ReadOnlyToTilelink(config: Axi4Config, bytesMax : Int) extends Component{
  val dp = Axi4ReadOnlyToTilelink.getTilelinkProposal(config, bytesMax)
  val io = new Bundle {
    val up = slave port Axi4ReadOnly(config)
    val down = master port tilelink.Bus(M2sParameters(dp, 1 << config.idWidth))
  }

  val a = new Area {
    io.down.a.arbitrationFrom(io.up.ar)

    val lenToSize = OHToUInt(OHMasking.last(io.up.ar.len) ## False)
    io.down.a.opcode := tilelink.Opcode.A.GET
    io.down.a.param := 0
    io.down.a.source := io.up.ar.id
    io.down.a.address := io.up.ar.addr
    io.down.a.size := (lenToSize + io.up.ar.size).resized
    io.down.a.debugId := 0
  }

  val d = new Area{
    io.up.r.arbitrationFrom(io.down.d)
    io.up.r.id := io.down.d.source
    io.up.r.data := io.down.d.data
    io.up.r.last := io.down.d.isLast()
    io.up.r.setOKAY()
    when(io.down.d.denied){io.up.r.setSLVERR()}
  }
}


//class Axi4ToTilelinkFull(config: Axi4Config,
//                         bytesMax : Int,
//                         slotsCount : Int,
//                         upPipe : StreamPipe = StreamPipe.NONE) extends Component{
//  val dp = Axi4ReadOnlyToTilelink.getTilelinkProposal(config, bytesMax)
//  val io = new Bundle {
//    val up = slave port Axi4(config)
//    val down = master port tilelink.Bus(M2sParameters(dp, 1 << config.idWidth))
//  }
//
//  val write = new Axi4WriteOnlyToTilelinkFull(config, bytesMax, slotsCount, upPipe)
//  val read = new Axi4ReadOnlyToTilelinkFull(config, bytesMax, slotsCount, upPipe)
//
//  write.io.up << io.up.toWriteOnly()
//  write.io.down << io.down.toWriteOnly()
//
//
//}