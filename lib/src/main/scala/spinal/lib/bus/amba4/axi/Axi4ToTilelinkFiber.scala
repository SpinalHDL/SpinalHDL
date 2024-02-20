package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.core.fiber._
import spinal.lib._
import spinal.lib.bus.tilelink
import spinal.lib.bus.tilelink.{M2sParameters, S2mSupport}

class Axi4ToTilelinkFiber(blockSize : Int, slotsCount : Int) extends Area {
  val up = Handle[Axi4]
  val downRead, downWrite = tilelink.fabric.Node.master()
  val down = tilelink.fabric.Node()
  down << (downRead, downWrite)


  val read = Fiber build new Area{
    var range = tilelink.SizeRange.upTo(blockSize)
    downRead.m2s.proposed load tilelink.M2sSupport(
      addressWidth = up.config.addressWidth,
      dataWidth = up.config.dataWidth,
      transfers = tilelink.M2sTransfers(
        get = range
      )
    )

//    range = tilelink.SizeRange.upTo(downRead.m2s.supported.transfers.sizeBytes)
    downRead.m2s.parameters load M2sParameters(
      sourceCount = slotsCount,
      support = downRead.m2s.proposed.copy(
        transfers = tilelink.M2sTransfers(
          get = range
        )
      )
    )
    downRead.s2m.supported load S2mSupport.none()
    val bridge = new Axi4ReadOnlyToTilelinkFull(
      up.config,
      bytesMax = range.max,
      slotsCount = slotsCount
    )
    bridge.io.up << up.toReadOnly()
    bridge.io.down >> downRead.bus
  }

  val write = Fiber build new Area {
    var writeRange = tilelink.SizeRange.upTo(blockSize)
    downWrite.m2s.proposed load tilelink.M2sSupport(
      addressWidth = up.config.addressWidth,
      dataWidth = up.config.dataWidth,
      transfers = tilelink.M2sTransfers(
        putFull = if (up.config.useAllStrb) writeRange else tilelink.SizeRange.none,
        putPartial = writeRange
      )
    )

//    writeRange = tilelink.SizeRange.upTo(downWrite.m2s.supported.transfers.sizeBytes)
    downWrite.m2s.parameters load M2sParameters(
      sourceCount = slotsCount,
      support = downWrite.m2s.proposed.copy(
        transfers = tilelink.M2sTransfers(
          putFull = if (up.config.useAllStrb) writeRange else tilelink.SizeRange.none,
          putPartial = writeRange
        )
      )
    )

    downWrite.s2m.supported load S2mSupport.none()
    val bridge = new Axi4WriteOnlyToTilelinkFull(
      up.config,
      bytesMax = writeRange.max,
      slotsCount = slotsCount
    )
    bridge.io.up << up.toWriteOnly()
    bridge.io.down >> downWrite.bus
  }
}
