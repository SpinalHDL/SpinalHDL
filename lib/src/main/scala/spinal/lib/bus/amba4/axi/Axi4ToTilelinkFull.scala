package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink._
import spinal.lib.eda.bench.{Bench, Rtl, XilinxStdTargets}



object Axi4WriteOnlyToTilelinkFull{
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

object Axi4ReadOnlyToTilelinkFull{
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

//Assume burst aligned and not more than 1 burst per id inflight
class Axi4WriteOnlyToTilelinkFull(val config: Axi4Config,
                                  val bytesMax : Int,
                                  val slotsCount : Int,
                                  val upPipe : StreamPipe = StreamPipe.NONE,
                                  val axiIdRemap : Option[Axi4OnePerIdRemapParam] = Option.empty[Axi4OnePerIdRemapParam],
                                  val boundaryWidth : Int = Axi4.boundaryWidth) extends Component{
  val ac = Axi4WriteOnlyAligner.getDownConfig(config, slotsCount)
  val dp = Axi4WriteOnlyToTilelinkFull.getTilelinkProposal(ac, bytesMax)
  val io = new Bundle {
    val up = slave port Axi4WriteOnly(config)
    val down = master port tilelink.Bus(M2sParameters(dp, slotsCount))
  }

  var onPerIdDown : Axi4WriteOnly = null
  val onPerId = axiIdRemap.isEmpty generate new Area{
    val bridge = new Axi4WriteOnlyOnePerId(config)
    bridge.io.up << io.up.pipelined(aw = upPipe)
    onPerIdDown = bridge.io.down
  }

  val onPerIdRemap = axiIdRemap.nonEmpty generate new Area{
    val bridge = new Axi4WriteOnlyOnePerIdRemap(config, axiIdRemap.get, boundaryWidth)
    bridge.io.up << io.up.pipelined(aw = upPipe)
    onPerIdDown = bridge.io.down
  }

  val compactor = new Axi4WriteOnlyCompactor(onPerIdDown.config)
  compactor.io.up << onPerIdDown

  val aligner = new Axi4WriteOnlyAligner(onPerIdDown.config, bytesMax, slotsCount, boundaryWidth = boundaryWidth)
  aligner.io.up << compactor.io.down

  val toTileink = new Axi4WriteOnlyToTilelink(ac, bytesMax)
  toTileink.io.up << aligner.io.down

  io.down << toTileink.io.down
}

object Axi4WriteOnlyToTilelinkFullGen extends App{
  val gen = SpinalVerilog(new Axi4WriteOnlyToTilelinkFull(
    Axi4Config(16, 32, 4),
    64,
    4
  ))

  Bench(List(Rtl(gen)), XilinxStdTargets("/media/data2/tools/xilinx/Vivado/2023.1/bin"))
}


//Assume burst aligned and not more than 1 burst per id inflight
class Axi4ReadOnlyToTilelinkFull(val config: Axi4Config,
                                 val bytesMax : Int,
                                 val slotsCount : Int,
                                 val upPipe : StreamPipe = StreamPipe.NONE,
                                 val axiIdRemap : Option[Axi4OnePerIdRemapParam] = Option.empty[Axi4OnePerIdRemapParam],
                                 val boundaryWidth : Int = Axi4.boundaryWidth) extends Component{
  val ac = Axi4ReadOnlyAligner.getDownConfig(config, slotsCount)
  val dp = Axi4ReadOnlyToTilelinkFull.getTilelinkProposal(ac, bytesMax)
  val io = new Bundle {
    val up = slave port Axi4ReadOnly(config)
    val down = master port tilelink.Bus(M2sParameters(dp, slotsCount))
  }

  var onPerIdDown : Axi4ReadOnly = null
  val onPerId = axiIdRemap.isEmpty generate new Area{
    val bridge = new Axi4ReadOnlyOnePerId(config)
    bridge.io.up << io.up.pipelined(ar = upPipe)
    onPerIdDown = bridge.io.down
  }

  val onPerIdRemap = axiIdRemap.nonEmpty generate new Area{
    val bridge = new Axi4ReadOnlyOnePerIdRemap(config, axiIdRemap.get, boundaryWidth = boundaryWidth)
    bridge.io.up << io.up.pipelined(ar = upPipe)
    onPerIdDown = bridge.io.down
  }

  val compactor = new Axi4ReadOnlyCompactor(onPerIdDown.config)
  compactor.io.up << onPerIdDown

  val aligner = new Axi4ReadOnlyAligner(onPerIdDown.config, bytesMax, slotsCount, boundaryWidth = boundaryWidth)
  aligner.io.up << compactor.io.down

  val toTileink = new Axi4ReadOnlyToTilelink(ac, bytesMax)
  toTileink.io.up << aligner.io.down

  io.down << toTileink.io.down
}

object Axi4ReadOnlyToTilelinkFullGen extends App{
  val gen = SpinalVerilog(new Axi4ReadOnlyToTilelinkFull(
    Axi4Config(16, 32, 4),
    64,
    4
  ))

  Bench(List(Rtl(gen)), XilinxStdTargets("/media/data2/tools/xilinx/Vivado/2023.1/bin"))
}

