package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

object Axi4IdRemover {
  def apply(axi: Axi4): Axi4 = {
    val idRemover = new Axi4IdRemover(axi.config)
    idRemover.io.input << axi
    idRemover.io.output
  }

  def apply(axi: Axi4Shared): Axi4Shared = {
    val idRemover = new Axi4SharedIdRemover(axi.config)
    idRemover.io.input << axi
    idRemover.io.output
  }

  def apply(axi: Axi4ReadOnly): Axi4ReadOnly = {
    val idRemover = new Axi4ReadOnlyIdRemover(axi.config)
    idRemover.io.input << axi
    idRemover.io.output
  }

  def apply(axi: Axi4WriteOnly): Axi4WriteOnly = {
    val idRemover = new Axi4WriteOnlyIdRemover(axi.config)
    idRemover.io.input << axi
    idRemover.io.output
  }
}

class Axi4IdRemover(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(new Axi4(config))
    val output = master(new Axi4(config.copy(useId = false, idWidth = -1)))
  }

  val readOnlyRemover = new Axi4ReadOnlyIdRemover(config)
  val writeOnlyRemover = new Axi4WriteOnlyIdRemover(config)

  readOnlyRemover.io.input << io.input
  writeOnlyRemover.io.input << io.input

  io.output << readOnlyRemover.io.output
  io.output << writeOnlyRemover.io.output
}

class Axi4SharedIdRemover(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(new Axi4Shared(config))
    val output = master(new Axi4Shared(config.copy(useId = false, idWidth = -1)))
  }

  if (!config.useId) {
    // Combo stage for naming
    io.output << io.input.pipelined(arw = StreamPipe.NONE,
      w = StreamPipe.NONE,
      b = StreamPipe.NONE,
      r = StreamPipe.NONE)
  } else {
    val txnActive = RegInit(False)
    txnActive setWhen(io.input.arw.fire)
    val txnId = RegNextWhen(io.input.arw.id, io.input.arw.fire)
    val txnWrite = RegNextWhen(io.input.arw.write, io.input.arw.fire)

    io.output.arw << io.input.arw.continueWhen(!txnActive)

    val readDone = (io.output.r.last && io.output.r.fire && !txnWrite)
    val writeDone = (io.output.b.fire && txnWrite)

    txnActive clearWhen(readDone || writeDone)

    io.input.r.arbitrationFrom(io.output.r)
    io.input.r.payload.assignSomeByName(io.output.r.payload)
    io.input.r.id := txnId

    io.output.w << io.input.w

    io.input.b.arbitrationFrom(io.output.b)
    io.input.b.payload.assignSomeByName(io.output.b.payload)
    io.input.b.id := txnId
  }
}

class Axi4ReadOnlyIdRemover(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(new Axi4ReadOnly(config))
    val output = master(new Axi4ReadOnly(config.copy(useId = false, idWidth = -1)))
  }

  if (!config.useId) {
    // Combo stage for naming
    io.output << io.input.pipelined(ar = StreamPipe.NONE,
                                    r = StreamPipe.NONE)
  } else {
    val txnActive = RegInit(False)
    txnActive setWhen(io.input.ar.fire)
    val txnId = RegNextWhen(io.input.ar.id, io.input.ar.fire)

    io.output.ar << io.input.ar.continueWhen(!txnActive)

    txnActive clearWhen(io.output.r.last && io.output.r.fire)

    io.input.r.arbitrationFrom(io.output.r)
    io.input.r.payload.assignSomeByName(io.output.r.payload)
    io.input.r.id := txnId
  }
}

class Axi4WriteOnlyIdRemover(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(new Axi4WriteOnly(config))
    val output = master(new Axi4WriteOnly(config.copy(useId = false, idWidth = -1)))
  }

  if (!config.useId) {
    // Combo stage for naming
    io.output << io.input.pipelined(aw = StreamPipe.NONE,
      w = StreamPipe.NONE,
      b = StreamPipe.NONE)
  } else {
    val txnActive = RegInit(False)
    txnActive setWhen(io.input.aw.fire)
    val txnId = RegNextWhen(io.input.aw.id, io.input.aw.fire)

    io.output.aw << io.input.aw.continueWhen(!txnActive)

    txnActive clearWhen(io.output.b.fire)

    io.output.w << io.input.w

    io.input.b.arbitrationFrom(io.output.b)
    io.input.b.payload.assignSomeByName(io.output.b.payload)
    io.input.b.id := txnId
  }
}