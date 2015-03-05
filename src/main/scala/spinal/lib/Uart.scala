package spinal.lib

import spinal._
import spinal.lib.UartConfig.{ParityType, StopType}

class Uart extends Bundle with Interface {
  val txd = Bool()
  val rxd = Bool()

  override def asMaster: this.type = {
    out(txd)
    in(rxd)
    this
  }
  override def asSlave: this.type = asMaster.flip
}


object UartConfig {

  object StopType extends SpinalEnum {
    val eStop1bit, eStop2bit = Value

    val toBitCount = SpinalMap(
      eStop1bit() -> UInt(0),
      eStop2bit() -> UInt(1)
    )
  }

  object ParityType extends SpinalEnum {
    val eParityNone, eParityEven, eParityOdd = Value
  }

}

class UartConfig(dataWidthMax: Int = 8) extends Bundle {
  val dataLength = UInt(log2Up(dataWidthMax) bit)
  val stop = StopType()
  val parity = ParityType()

  val clockDivider = UInt(20 bit)
}


class UartTx(dataWidthMax: Int = 8) extends Component {
  val io = new Bundle {
    val config = in(new UartConfig(dataWidthMax))
    val cmd = slave Handshake (Bits(dataWidthMax bit))
    val txd = out Bool()
  }

  val timer = new Area {
    val counter = Reg(io.config.clockDivider);
    val reset = Bool()
    val tick = counter === UInt(0)

    counter := counter - UInt(1)
    when(tick || reset) {
      counter := io.config.clockDivider
    }
  }

  val tickCounter = new Area {
    val value = Reg(UInt(Math.max(dataWidthMax, 2) bit));
    val reset = Bool()

    when(timer.tick) {
      value := value + UInt(1)
    }
    when(reset) {
      value := UInt(0)
    }
  }

  val stateMachine = new Area {

    object State extends SpinalEnum {
      val idle, start, data, parity, stop = Value
    }

    val state = RegInit(State.idle())
    val paritySum = Reg(Bool());
    val dataBuffer = Reg(io.cmd.data)

    val lookingForJob = Bool()
    val txd = Bool()

    when(timer.tick) {
      paritySum := paritySum ^ txd
    }

    lookingForJob := Bool(false)
    tickCounter.reset := Bool(false)
    timer.reset := Bool(false)
    txd := Bool(true)
    switch(state) {
      is(State.idle) {
        lookingForJob := Bool(true)
      }
      is(State.start) {
        txd := Bool(false)
        when(timer.tick) {
          state := State.data
          paritySum := io.config.parity === ParityType.eParityOdd
          tickCounter.reset := Bool(true)
        }
      }
      is(State.data) {
        txd := dataBuffer(tickCounter.value)
        when(timer.tick) {
          when(tickCounter.value === io.config.dataLength) {
            tickCounter.reset := Bool(true)
            when(io.config.parity === ParityType.eParityNone) {
              state := State.stop
            } otherwise {
              state := State.parity
            }
          }
        }
      }
      is(State.parity) {
        txd := paritySum
        when(timer.tick) {
          state := State.stop
          tickCounter.reset := Bool(true)
        }
      }
      is(State.stop) {
        when(timer.tick) {
          when(tickCounter.value === StopType.toBitCount(io.config.stop)) {
            state := State.idle
            lookingForJob := Bool(true)
          }
        }
      }
    }

    io.cmd.ready := Bool(false)
    when(lookingForJob && io.cmd.valid) {
      io.cmd.ready := Bool(true)
      timer.reset := Bool(true)
      dataBuffer := io.cmd.data
      state := State.start
    }
  }

  io.txd := RegNext(stateMachine.txd, Bool(true))

}


class UartRx(dataWidthMax: Int = 8, sampleCount: Int = 6, transitionSize: Int = 2) extends Component {
  val io = new Bundle {
    val config = in(new UartConfig(dataWidthMax))
    val cmd = master Flow (Bits(dataWidthMax bit))
    val rxd = in Bool()
  }

  val clockDivider = new Area {
    val counter = Reg(io.config.clockDivider)
    val tick = counter === UInt(0)

    counter := counter - UInt(1)
    when(tick) {
      counter := io.config.clockDivider
    }
  }

  val sampler = new Area {
    val frontBuffer = CCBuffer(io.rxd)
    val samples = RegInit(BitsSet(sampleCount bit))
    when(clockDivider.tick) {
      samples := samples ## frontBuffer
    }
    val value = RegNext(MajorityVote(samples))
    val event = RegNext(clockDivider.tick)
  }

  val baud = new Area {
    val counter = Reg(UInt(log2Up(sampleCount + transitionSize)))
    val tick = counter === UInt(0)

    when(sampler.event) {
      counter := counter - UInt(1)
    }
    when(tick) {
      counter := UInt(sampleCount + transitionSize - 1)
    }

    //TODO verify UInt(sampleCount / 2 - 1)
    def reset: Unit = counter := UInt(sampleCount / 2 - 1)
    def value = sampler.value
  }


  val baudCounter = new Area {
    val value = Reg(UInt(Math.max(dataWidthMax, 2) bit))
    def reset: Unit = value := UInt(0)

    when(baud.tick) {
      value := value + UInt(1)
    }
  }

  val stateMachine = new Area {
    object State extends SpinalEnum {
      val idle, start, data, parity, stop = Value
    }

    val state = RegInit(State.idle())
    val paritySum = Reg(Bool())
    val dataBuffer = Reg(io.cmd.data)

    when(baud.tick) {
      paritySum := paritySum ^ baud.value
    }

    io.cmd.valid := Bool(false)
    switch(state) {
      is(State.idle) {
        when(baud.value === Bool(false)) {
          state := State.start
          baud.reset
        }
      }
      is(State.start) {
        when(baud.tick) {
          state := State.data
          baudCounter.reset
          paritySum := io.config.parity === ParityType.eParityOdd
          when(baud.value === Bool(true)) {
            state := State.idle
          }
        }
      }
      is(State.data) {
        when(baud.tick) {
          dataBuffer(baudCounter.value) := baud.value
          when(baudCounter.value === io.config.dataLength) {
            baudCounter.reset
            when(io.config.parity === ParityType.eParityNone) {
              state := State.stop
            } otherwise {
              state := State.parity
            }
          }
        }
      }
      is(State.parity) {
        when(baud.tick) {
          state := State.stop
          baudCounter.reset
          when(paritySum === baud.value) {
            state := State.idle
          }
        }
      }
      is(State.stop) {
        when(baud.tick) {
          when(!baud.value) {
            state := State.idle
          }.elsewhen(baudCounter.value === StopType.toBitCount(io.config.stop)) {
            state := State.idle
            io.cmd.valid := Bool(true)
          }
        }
      }
    }
  }
  io.cmd.data := stateMachine.dataBuffer
}