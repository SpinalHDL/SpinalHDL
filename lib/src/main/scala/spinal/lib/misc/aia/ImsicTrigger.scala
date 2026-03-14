package spinal.lib.misc.aia

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

case class ImsicTriggerMapper(sourceIds: Seq[Int], hartId: Int, guestId: Int) extends Component {
  val registerWidth = 32

  val idWidth = log2Up((sourceIds ++ Seq(0)).max + 1)
  require(idWidth <= registerWidth)

  val io = new Bundle {
    val drivers = in Vec(Flow(UInt(registerWidth bits)), Flow(UInt(registerWidth bits)))
    val triggers = out(Bits(sourceIds.size bits))
  }

  case class ImsicSource(sourceId: Int) extends Area {
    val id = U(sourceId, registerWidth bits)
    val trigger = Bool()

    trigger := False
  }

  val sources = for (sourceId <- sourceIds) yield new ImsicSource(sourceId)

  io.triggers := sources.map(_.trigger).asBits()

  /* Main work, mapping the irq set */
  val triggers = for (driver <- io.drivers) yield new Area {
    when(driver.valid) {
      for (source <- sources) {
        when (driver.payload === source.id) {
          source.trigger := True
        }
      }
    }
  }

  def driveFrom(bus: BusSlaveFactory, baseAddress: BigInt) = new Area {
    val SETEIPNUM_LE_ADDR = 0x000
    val SETEIPNUM_BE_ADDR = 0x004

    val busWithOffset = new BusSlaveFactoryAddressWrapper(bus, baseAddress)
    val targetDriveLE = busWithOffset.createAndDriveFlow(UInt(registerWidth bits), address = SETEIPNUM_LE_ADDR, documentation = "Set External Interrupt-Pending bit for %s of hart %d by Little-Endian Number".format(if (guestId == 0) "non-guest" else s"guest ${guestId}", hartId))

    val targetDriveBE = busWithOffset.createAndDriveFlow(UInt(registerWidth bits), address = SETEIPNUM_BE_ADDR, documentation = "Set External Interrupt-Pending bit for %s of hart %d by Big-Endian Number".format(if (guestId == 0) "non-guest" else s"guest ${guestId}", hartId))

    io.drivers := Vec(targetDriveLE, targetDriveBE.map(EndiannessSwap(_)))
  }
}

/**
 * ImsicMapping: IMSIC interrupt file mapping info
 *
 * Each interrupt file address should be calculated as below:
 * g * 2^E + B + h * 2^D
 *
 * g is the IMSIC group id and the h is the hart id in the group
 *
 * @interruptFileHartSize:
 *   The interrupt file size for one hart, for IMSIC supports guest
 *   interrupt file, this size should cover all guest interrupt
 *   file. This argument is 2^D in the address formula.
 * @interruptFileHartOffset:
 *   The offset of the interrupt file size for one hart. This
 *   argument is B in the address formula.
 * @interruptFileGroupSize:
 *   The group size for one interrupt file group, This argument is
 *   2^E in the address formula.
 *
 * For convenient, all the arguments could be set to zero for auto
 * calculation. But when `interruptFileHartOffset` is not zero,
 * `interruptFileGroupSize` must be set non-zero, or the calculation
 * will fail.
 *
 */
case class ImsicMapping(
  interruptFileHartSize       : BigInt = 0,
  interruptFileHartOffset     : BigInt = 0,
  interruptFileGroupSize      : BigInt = 0
)

object ImsicTrigger {
  val interruptFileSize: BigInt = 4096

  def mappingCalibrate(mapping: ImsicMapping, maxGuestId: Int, maxGroupHartId: Int, maxGroupId: Int): ImsicMapping = {
    import mapping._

    require(interruptFileHartSize == 0 || isPow2(interruptFileHartSize), "interruptFileHartSize should be power of 2")
    require(interruptFileGroupSize == 0 || isPow2(interruptFileGroupSize), "interruptFileGroupSize should be power of 2")
    require(!(interruptFileHartOffset != 0 && interruptFileGroupSize == 0), "Can not auto calculate interruptFileGroupSize when interruptFileHartOffset != 0")

    require(maxGuestId < 16, "Per hart can only have max 15 guest interrupt files.")

    val intFileNumber = 1 << log2Up(maxGuestId + 1)
    val minIntFileHartSize = interruptFileSize * intFileNumber
    val realIntFileHartSize = if (interruptFileHartSize != 0) interruptFileHartSize else minIntFileHartSize
    require(realIntFileHartSize >= minIntFileHartSize)

    val intFileGroupHarts = 1 << log2Up(maxGroupHartId + 1)
    val minIntFileGroupSize = realIntFileHartSize * intFileGroupHarts
    val realIntFileGroupSize = if (interruptFileGroupSize != 0) interruptFileGroupSize else minIntFileGroupSize
    require(realIntFileGroupSize >= minIntFileGroupSize)

    val intFileGroupMask = minIntFileGroupSize - 1
    val intFileGroupIdMask = 1 << log2Up(maxGroupId + 1) - 1
    val intFileTestMask = intFileGroupMask + realIntFileGroupSize * intFileGroupIdMask
    require((interruptFileHartOffset & intFileTestMask) == 0, "interruptFileHartOffset should not cover any interrupt file")

    return ImsicMapping(
      interruptFileHartSize   = realIntFileHartSize,
      interruptFileHartOffset = interruptFileHartOffset,
      interruptFileGroupSize  = realIntFileGroupSize
    )
  }

  def imsicOffset(mapping: ImsicMapping, groupId: Int, groupHartId: Int, guestId: Int) = {
    import mapping._

    val offset = groupId * interruptFileGroupSize +
                 groupHartId * interruptFileHartSize +
                 interruptFileHartOffset +
                 guestId * interruptFileSize

    offset
  }

  def apply(bus: BusSlaveFactory, mapping: ImsicMapping)(infos: Seq[ImsicFileInfo]) = new Area {
    val maxGuestId = infos.map(_.guestId).max
    val maxGroupHartId = infos.map(_.groupHartId).max
    val maxGroupId = infos.map(_.groupId).max

    val realMapping = mappingCalibrate(mapping, maxGuestId, maxGroupHartId, maxGroupId)

    val mappers = for (info <- infos) yield new Area {
      val mapper = ImsicTriggerMapper(info.sourceIds, info.hartId, info.guestId)
      val offset = imsicOffset(realMapping, info.groupId, info.groupHartId, info.guestId)

      mapper.driveFrom(bus, offset)

      setName(f"trigger_g${info.groupId}h${info.groupHartId}v${info.guestId}")
    }

    val triggers = Vec(mappers.map(_.mapper.io.triggers))
  }

  def addressWidth(mapping: ImsicMapping, maxGuestId: Int, maxGroupHartId: Int, maxGroupId: Int): Int = {
    val realMapping = mappingCalibrate(mapping, maxGuestId, maxGroupHartId, maxGroupId)

    val intFileNumber = 1 << log2Up(maxGuestId + 1)
    val intFileGroupHarts = 1 << log2Up(maxGroupHartId + 1)
    val intFileGroupMax = 1 << log2Up(maxGroupId + 1)

    val ImsicSize = imsicOffset(realMapping, maxGroupId, maxGroupHartId, maxGuestId + 1)

    return log2Up(ImsicSize)
  }
}
