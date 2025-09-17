package spinal.lib.misc.plic

import spinal.core._
import spinal.lib.bus.amba4.axilite._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

class Axi4Plic(axi4Config : Axi4Config, sourceCount  : Int, targetCount : Int) extends Component{
  val priorityWidth = 2
  val plicMapping = PlicMapping.sifive
  import plicMapping._

  val io = new Bundle {
    val bus = slave(Axi4(axi4Config))
    val sources = in Bits (sourceCount bits)
    val targets = out Bits (targetCount bits)
  }

  val gateways = (for ((source, id) <- (io.sources.asBools, 1 to sourceCount).zipped) yield PlicGatewayActiveHigh(
    source = source,
    id = id,
    priorityWidth = priorityWidth
  )).toSeq

  val targets = for (i <- 0 until targetCount) yield PlicTarget(
    id = i,
    gateways = gateways,
    priorityWidth = priorityWidth
  )

  io.targets := targets.map(_.iep).asBits

  val bus = new Axi4SlaveFactory(io.bus)
  val mapping = PlicMapper(bus, plicMapping)(
    gateways = gateways,
    targets = targets
  )
}

object Axi4Plic extends App{
  var axi4Config = Axi4Config(
    addressWidth = 32,
    dataWidth = 32,
    idWidth = 8,
    useRegion = false,
    useLock = false,
    useCache = false,
    useQos = false,
    useProt = false
  )
  var sourceCount = 31
  var targetCount = 1

  var sConfig = SpinalConfig()

  assert(new scopt.OptionParser[Unit]("VexRiscvLitexSmpClusterCmdGen") {
    help("help").text("prints this usage text")
    opt[Int]("source-count") action { (v, c) => sourceCount = v }
    opt[Int]("target-count") action { (v, c) => targetCount = v }
    opt[Int]("axi-id-width") action { (v, c) => axi4Config = axi4Config.copy(idWidth = v) }
  }.parse(args, ()).isDefined)

  sConfig.generateVerilog(new Axi4Plic(axi4Config, sourceCount, targetCount))
}