package spinal.lib.misc.aia

import spinal.core._
import spinal.core.fiber.{Fiber, Lock}
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.tilelink

import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}

class MappedImsicTrigger[T <: spinal.core.Data with IMasterSlave](infos: Seq[ImsicFileInfo],
                                                           mapping: ImsicMapping,
                                                           busType: HardType[T],
                                                           factoryGen: T => BusSlaveFactory) extends Component {
  val io = new Bundle {
    val bus = slave(busType())
    val triggers = Vec(infos.map(info => master Stream(UInt(ImsicTriggerMapper.registerWidth bits))))
  }

  val factory = factoryGen(io.bus)

  val logic = ImsicTrigger(factory, mapping)(infos)

  io.triggers.zip(logic.triggers).foreach(t => t._1 << t._2)
}

case class TilelinkImsicTrigger(infos: Seq[ImsicFileInfo],
                                mapping: ImsicMapping,
                                p: bus.tilelink.BusParameter) extends MappedImsicTrigger[bus.tilelink.Bus](
  infos,
  mapping,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object TilelinkImsicTrigger {
  def getTilelinkSupport(transfers: tilelink.M2sTransfers, addressWidth: Int = 20) = bus.tilelink.SlaveFactory.getSupported(
    addressWidth = addressWidth,
    dataWidth = 32,
    allowBurst = false,
    proposed = tilelink.M2sSupport(
      addressWidth = addressWidth,
      dataWidth = 32,
      transfers = transfers
    )
  )
  def getTilelinkSupport(transfers: tilelink.M2sTransfers, mapping: ImsicMapping, infos: Seq[ImsicFileInfo]): tilelink.M2sSupport = getTilelinkSupport(transfers, addressWidth(mapping, infos))

  def addressWidth(mapping: ImsicMapping, infos: Seq[ImsicFileInfo]): Int = {
    val maxGuestId = infos.map(_.guestId).max
    val maxGroupHartId = infos.map(_.groupHartId).max
    val maxGroupId = infos.map(_.groupId).max

    ImsicTrigger.addressWidth(mapping, maxGuestId, maxGroupHartId, maxGroupId)
  }
}

class MappedCoreImsicTrigger[T <: spinal.core.Data with IMasterSlave](
  infos: Seq[ImsicFileInfo],
  busType: HardType[T],
  factoryGen: T => BusSlaveFactory
) extends Component {
  val io = new Bundle {
    val bus = slave(busType())
    val triggers = Vec(infos.map(info => master Stream(UInt(ImsicTriggerMapper.registerWidth bits))))
  }

  val factory = factoryGen(io.bus)
  val logic = ImsicTrigger(factory)(infos)

  io.triggers.zip(logic.triggers).foreach(t => t._1 << t._2)
}

case class TilelinkCoreImsicTrigger(infos: Seq[ImsicFileInfo],
                                    p: bus.tilelink.BusParameter) extends MappedCoreImsicTrigger[bus.tilelink.Bus](
  infos,
  new bus.tilelink.Bus(p),
  new bus.tilelink.SlaveFactory(_, true)
)

object TilelinkCoreImsicTrigger {
  def getTilelinkSupport(transfers: tilelink.M2sTransfers, infos: Seq[ImsicFileInfo]) = TilelinkImsicTrigger.getTilelinkSupport(transfers, addressWidth(infos))

  def addressWidth(infos: Seq[ImsicFileInfo]): Int = {
    val guestIdWidth = log2Up(infos.map(_.guestId).max + 1)

    12 + guestIdWidth
  }
}

case class TilelinkCoreImsicTriggerFiber() extends Area {
  val node = bus.tilelink.fabric.Node.slave()
  val lock = Lock()

  case class ImsicFileSource(info: ImsicFileInfo) {
    val trigger = Stream(UInt(ImsicTriggerMapper.registerWidth bits))
  }
  var sources = ArrayBuffer[ImsicFileSource]()
  def addImsicFileinfo(info: ImsicFileInfo) = {
    val source = sources.addRet(ImsicFileSource(info))
    source.trigger
  }

  val thread = Fiber build new Area {
    lock.await()

    val infos = sources.map(_.info).toSeq

    node.m2s.supported.load(TilelinkCoreImsicTrigger.getTilelinkSupport(node.m2s.proposed.transfers, infos))
    node.s2m.none()

    val core = TilelinkCoreImsicTrigger(infos, node.bus.p)

    core.io.bus <> node.bus
    for ((source, trigger) <- sources.zip(core.io.triggers)) {
      source.trigger << trigger
    }
  }
}

case class TilelinkImsicTriggerFiber(mapping: ImsicMapping = ImsicMapping()) extends Area {
  val node = bus.tilelink.fabric.Node()
  val lock = Lock()

  var cores = LinkedHashMap[Int, TilelinkCoreImsicTriggerFiber]()
  def addImsicFileinfo(info: ImsicFileInfo) = {
    val core = cores.getOrElseUpdate(info.hartId, TilelinkCoreImsicTriggerFiber())
    core.addImsicFileinfo(info)
  }

  val connect = Fiber setup new Area {
    lock.await()

    val imsicInfos = cores.map(_._2.sources.map(_.info)).flatten.toSeq
    val realMapping = ImsicTrigger.mappingCalibrate(mapping, imsicInfos)

    for ((id, core) <- cores) {
      assert(core.sources.size > 0)

      val info = core.sources(0).info
      core.setName(f"${getName()}_core_g${info.groupId}h${info.groupHartId}")
      core.node at ImsicTrigger.imsicGroupHartOffset(realMapping, info) of node
    }
  }
}
