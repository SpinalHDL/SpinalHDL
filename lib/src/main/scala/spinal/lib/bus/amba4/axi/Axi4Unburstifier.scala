package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

object Axi4Unburstifier {
  def apply(axi: Axi4): Axi4 = {
    val roUnburstifier = new Axi4ReadOnlyUnburstifier(axi.config)
    val woUnburstifier = new Axi4WriteOnlyUnbustifier(axi.config)

    roUnburstifier.io.input << axi.toReadOnly()
    woUnburstifier.io.input << axi.toWriteOnly()

    val axiUnburst = new Axi4(axi.config.copy(useLen = false))
    axiUnburst << roUnburstifier.io.output
    axiUnburst << woUnburstifier.io.output
    axiUnburst
  }

  def apply(axi: Axi4ReadOnly): Axi4ReadOnly = {
    val unburstifier = new Axi4ReadOnlyUnburstifier(axi.config)
    unburstifier.io.input << axi
    unburstifier.io.output
  }

  def apply(axi: Axi4WriteOnly): Axi4WriteOnly = {
    val unburstifier = new Axi4WriteOnlyUnbustifier(axi.config)
    unburstifier.io.input << axi
    unburstifier.io.output
  }
}

class Axi4ReadOnlyUnburstifier(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(Axi4ReadOnly(config))
    val output = master(Axi4ReadOnly(config.copy(useLen = false, useBurst = false, useSize = false, useLast = false)))
  }

  val unburstified = io.input.ar.unburstify

  io.output.ar.arbitrationFrom(unburstified)
  io.output.ar.payload.assignSomeByName(unburstified.fragment)

  io.input.r.arbitrationFrom(io.output.r)
  io.input.r.payload.assignSomeByName(io.output.r.payload)

  if (config.useLast) {
    val addrBeats = Reg(UInt(8 bit))
    val active = RegInit(False)

    when(io.input.ar.fire) {
      addrBeats := io.input.ar.len
      active set()
    }

    when(io.input.r.fire) {
      addrBeats := addrBeats - 1
    }
    io.input.r.last := addrBeats === 0
  }
}

class Axi4WriteOnlyUnbustifier(config: Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(Axi4WriteOnly(config))
    val output = master(Axi4WriteOnly(config.copy(useLen = false, useBurst = false, useSize = false)))
  }

  val unburstified = io.input.aw.unburstify

  val addrBeats = Reg(UInt(8 bit))
  val active = RegInit(False)
  val done = RegInit(False)
  val resp = if (config.useResp) Reg(Bits(2 bit)) else null

  io.output.aw.arbitrationFrom(unburstified)
  io.output.aw.payload.assignSomeByName(unburstified.fragment)

  when(io.input.aw.fire) {
    addrBeats := io.input.aw.len
    active := True
    done clear()
    config.useResp generate resp.clearAll()
  }

  io.output.w << io.input.w

  io.output.b.ready := active

  when(io.output.b.fire) {
    addrBeats := addrBeats - 1
    done setWhen(addrBeats === 0)
    config.useResp generate {
      when(resp.orR) {
        resp := io.output.b.resp
      }
    }
  }

  io.input.b.valid := done
  config.useResp generate { io.input.b.resp := resp }

  active clearWhen (io.input.b.fire)
  done clearWhen (io.input.b.fire)
}
