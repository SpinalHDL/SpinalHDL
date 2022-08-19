package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

object Axi4Unburster {
  def apply(axi: Axi4): Axi4 = {
    val roUnburstifier = new Axi4ReadOnlyUnburster(axi.config)
    val woUnburstifier = new Axi4WriteOnlyUnburster(axi.config)

    roUnburstifier.io.input << axi.toReadOnly()
    woUnburstifier.io.input << axi.toWriteOnly()

    val axiUnburst = new Axi4(axi.config.copy(useLen = false))
    axiUnburst << roUnburstifier.io.output
    axiUnburst << woUnburstifier.io.output
    axiUnburst
  }

  def apply(axi: Axi4ReadOnly): Axi4ReadOnly = {
    val unburstifier = new Axi4ReadOnlyUnburster(axi.config)
    unburstifier.io.input << axi
    unburstifier.io.output
  }

  def apply(axi: Axi4WriteOnly): Axi4WriteOnly = {
    val unburstifier = new Axi4WriteOnlyUnburster(axi.config)
    unburstifier.io.input << axi
    unburstifier.io.output
  }
}

class UnbursterIDManager(config: Axi4Config, pendingDepth: Int = 3, pendingWidth: Int = 3) extends Component {
  class IdLen extends Bundle {
    val id = (config.useId) generate config.idType
    val len = config.lenType
  }

  class IdResp extends Bundle {
    val id = (config.useId) generate config.idType
    val resp = Bits(2 bit)
  }

  val io = new Bundle {
    val axIdLen = slave(Stream(new IdLen))
    val retIdResp = slave(Stream(new IdResp))
//    val drop = out Bool()
    val resp = out Bits(2 bit)
    val last = out Bool()
  }

  if (config.useId) {

    val idMap = Vec(Reg(config.idType) init(0), pendingWidth)
    val idMapValid = Bits(pendingWidth bit)
    val lenFifos = (for (_ <- 0 until pendingWidth) yield new StreamFifo(config.lenType, pendingDepth).io).toList

    val push = new Area {
      val idMapMatches = Bits(pendingWidth bit)
      val idMapOccupied = Bits(pendingWidth bit)
      for(i <- 0 until pendingWidth) {
        val idMatch = (idMap(i) === io.axIdLen.id)
        idMapOccupied(i) := lenFifos(i).occupancy.orR || idMapValid(i)
        idMapMatches(i) := idMatch && idMapOccupied(i)
      }

      val matchedIdIndex = OHMasking.first(idMapMatches)
      val anyMatched = idMapMatches.orR
      val nextOpenIndex = OHMasking.first(~idMapOccupied)
      val anyOpen = (~idMapOccupied).orR

      io.axIdLen.ready := False
      for (i <- 0 until pendingWidth) {
        lenFifos(i).push.payload := io.axIdLen.len
        lenFifos(i).push.valid := False
        when(anyMatched) {
          when(matchedIdIndex(i)) {
            // Connect AR fork to existing FIFO
            io.axIdLen.ready := lenFifos(i).push.ready
            lenFifos(i).push.valid := io.axIdLen.valid
          }
        } elsewhen (anyOpen && nextOpenIndex(i)) {
          // Connect AR fork to open FIFO
          io.axIdLen.ready := lenFifos(i).push.ready
          lenFifos(i).push.valid := io.axIdLen.valid
        }

        when(io.axIdLen.fire) {
          when(!anyMatched && anyOpen && nextOpenIndex(i)) {
            idMap(i) := io.axIdLen.id
          }
        }
      }
    }

    val pop = new Area {

      val respOut = Bits(2 bit)
      val respOutReg = RegNextWhen(respOut, io.retIdResp.fire)

      //      io.retIdResp.ready := False
      io.last := False
      io.resp := respOut

      respOut := respOutReg

      val idMapMatches = Bits(pendingWidth bit)
      for(i <- 0 until pendingWidth) {
        val idMatch = (idMap(i) === io.retIdResp.id)
        idMapMatches(i) := idMatch
      }
      idMapMatches.dontSimplifyIt()

//      io.drop := !idMapMatches.orR

      io.retIdResp.ready := False //idMapValid.orR

      val select = OHMasking.first(idMapMatches)

      for(i <- 0 until pendingWidth) {
        val lenFifo = lenFifos(i).pop
        val valid = RegInit(False)
        valid.dontSimplifyIt()
        val addrBeats = Reg(config.lenType.clone) init(0)
        addrBeats.dontSimplifyIt()
        val resp = Reg(Bits(2 bit)) init(0)

        idMapValid(i) := valid

        lenFifo.ready := !valid

        // Load valid
        when(lenFifo.fire) {
          addrBeats := lenFifo.payload
          resp.clearAll()
          valid.set()
        }

        when(select(i) && valid) {
          respOut := resp

          io.retIdResp.ready := valid

          when(io.retIdResp.fire) {
            addrBeats := addrBeats - 1
            when(!resp.orR) {
              resp := io.retIdResp.resp
              respOut := io.retIdResp.resp
            }
          }

          valid.clearWhen(io.retIdResp.fire && addrBeats === 0)
          io.last := addrBeats === 0
        }
      }
    }

  } else {
    // Length FIFO
    val fifo = new StreamFifo(config.lenType.clone, pendingDepth)
    val lenInStream = fifo.io.push
    val lenFifo = fifo.io.pop

    // Working length
    val valid = RegInit(False)
    val addrBeats = Reg(UInt(8 bit)) init(0)
    val resp = Reg(Bits(2 bit)) init(0)

    // Fork into the length FIFO
    io.axIdLen.ready := lenInStream.ready
    lenInStream.valid := io.axIdLen.valid
    lenInStream.payload := io.axIdLen.len

    lenFifo.ready := !valid

    when(lenFifo.fire) {
      addrBeats := lenFifo.payload
      resp.clearAll()
      valid.set()
    }

    when(io.retIdResp.fire) {
      addrBeats := addrBeats - 1
      when(!resp.orR) {
        resp := io.retIdResp.resp
      }
    }

    valid.clearWhen(io.retIdResp.fire && addrBeats === 0)
    io.retIdResp.ready := valid
    io.last := addrBeats === 0
    io.resp := resp

//    io.drop := !(valid || fifo.io.occupancy.orR)
  }
}

/**
  * Converts a burst transaction into single beat transactions.
  *
  * Notice: This component only supports a single transaction at a time.
  *
  * TODO: Support concurrent transactions
  */

/**
  * Converts Axi4 burst streams into single beat transactions and adds last as required.
  * AR channel will block if the pending transactions FIFO is full for that ID.
  *
  * @param config Axi4Config of the inbound master stream
  * @param pendingDepth Number of pending transactions per ID
  * @param pendingWidth Number of concurrent pending ID transactions.
  *                     Not used if the master stream does not support IDs.
  */
class Axi4ReadOnlyUnburster(config: Axi4Config, pendingDepth: Int = 3, pendingWidth: Int = 3) extends Component {
  val io = new Bundle {
    val input = slave(Axi4ReadOnly(config))
    val output = master(Axi4ReadOnly(config.copy(useLen = false, useBurst = false)))
  }

  if (!config.useLast) {
    val unburstified = io.input.ar.unburstify

    io.output.ar.arbitrationFrom(unburstified)
    io.output.ar.payload.assignSomeByName(unburstified.fragment)

    io.input.r.arbitrationFrom(io.output.r)
    io.input.r.payload.assignSomeByName(io.output.r.payload)
  } else {

    val manager = new UnbursterIDManager(config, pendingDepth, pendingWidth)

    val unburstified = io.input.ar.continueWhen(manager.io.axIdLen.ready).unburstify
    manager.io.axIdLen.valid := io.input.ar.fire
    (config.useId) generate { manager.io.axIdLen.id := io.input.ar.id }
    manager.io.axIdLen.len := io.input.ar.len

    io.output.ar.arbitrationFrom(unburstified)
    io.output.ar.payload.assignSomeByName(unburstified.fragment)

    val rFifo = io.output.r.queueLowLatency(8).continueWhen(manager.io.retIdResp.ready)
    (config.useId) generate { manager.io.retIdResp.id := rFifo.id }
    manager.io.retIdResp.valid := rFifo.fire
    manager.io.retIdResp.resp.clearAll()
    rFifo.last.allowOverride
    rFifo.last := manager.io.last

    val rStage = rFifo.stage()

    io.input.r.arbitrationFrom(rStage)
    io.input.r.payload.assignSomeByName(rStage.payload)
  }
}

/**
  * Converts a single burst transaction into single beat transactions and merges write responses.
  *
  * Notice: This component only supports a single transaction at a time.
  *
  * TODO: Support concurrent transactions
  */
class Axi4WriteOnlyUnburster(config: Axi4Config, pendingDepth: Int = 3, pendingWidth: Int = 3) extends Component {
  val io = new Bundle {
    val input = slave(Axi4WriteOnly(config))
    val output = master(Axi4WriteOnly(config.copy(useLen = false, useBurst = false)))
  }

  val manager = new UnbursterIDManager(config, pendingDepth, pendingWidth)

  val unburstified = io.input.aw.continueWhen(manager.io.axIdLen.ready).unburstify
  manager.io.axIdLen.valid := io.input.aw.fire
  (config.useId) generate { manager.io.axIdLen.id := io.input.aw.id }
  manager.io.axIdLen.len := io.input.aw.len

  io.output.aw.arbitrationFrom(unburstified)
  io.output.aw.payload.assignSomeByName(unburstified.fragment)

  io.output.w << io.input.w
  if (config.useLast) {
    io.output.w.last.allowOverride
    io.output.w.last := True
  }

  val bFifo = io.output.b.queueLowLatency(8).continueWhen(manager.io.retIdResp.ready)
  (config.useId) generate { manager.io.retIdResp.id := bFifo.id }
  manager.io.retIdResp.valid := bFifo.fire
  if(config.useResp) {
    manager.io.retIdResp.resp := bFifo.resp
  } else {
    manager.io.retIdResp.resp.clearAll()
  }

  val bStage = bFifo.takeWhen(manager.io.last).stage()

  io.input.b.arbitrationFrom(bStage)
  io.input.b.payload.assignSomeByName(bStage.payload)
}
