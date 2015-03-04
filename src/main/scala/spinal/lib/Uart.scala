package spinal.lib

import spinal._
import spinal.lib.UartConfig.{ParityType, StopType}

class Uart extends Bundle with Interface {
  val txd = Bool()
  val rxd = Bool()

  override def asMaster: Unit = {
    out(txd)
    in(rxd)
  }
  override def asSlave: Unit = {
    in(txd)
    out(rxd)
  }
}


object UartConfig {


  //  object DataType extends SpinalEnum {
  //    val eData7bit, eData8bit, eData9bit = Value
  //
  //    val toBitCount = Map(
  //      eData7bit -> UInt(7),
  //      eData8bit -> UInt(8),
  //      eData9bit -> UInt(9))
  //  }

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
    val cmd = slave Handshake (Bits(dataWidthMax bit));
    val txd = out Bool();
  }

  object State extends SpinalEnum {
    val idle, start, data, parity, stop = Value
  }

  val timer = new Area {
    val counter = Reg(io.config.clockDivider);
    val reset = Bool()
    val tick = Bool()

    tick := Bool(false)
    counter := counter - UInt(1)
    when(counter === UInt(0) || reset) {
      counter := io.config.clockDivider
      tick := Bool(true)
    }
  }


  val state = RegInit(State.idle())
  val counter = Reg(UInt(4 bit));
  val counterReset = Bool()
  val paritySum = Reg(Bool());
  val lockingForJob = Bool()
  val dataBuffer = Reg(io.cmd.data)

  val txd = Bool()

  when(timer.tick) {
    counter := counter + UInt(1)
  }
  when(counterReset) {
    counter := UInt(0)
  }
  when(timer.tick) {
    paritySum := paritySum ^ txd
  }


  lockingForJob := Bool(false)
  counterReset := Bool(false)
  timer.reset := Bool(false)
  txd := Bool(true)
  switch(state) {
    is(State.idle) {
      lockingForJob := Bool(true)
    }
    is(State.start) {
      txd := Bool(false)
      when(timer.tick) {
        state := State.data
        paritySum := io.config.parity === ParityType.eParityOdd
        counterReset := Bool(true)
      }
    }
    is(State.data) {
      txd := dataBuffer(counter)
      when(timer.tick) {
        when(counter === io.config.dataLength) {
          counterReset := Bool(true)
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
        counterReset := Bool(true)
      }
    }
    is(State.stop) {
      when(timer.tick) {
        when(counter === StopType.toBitCount(io.config.stop)) {
          state := State.idle
          lockingForJob := Bool(true)
        }
      }
    }
  }

  io.cmd.ready := Bool(false)
  when(lockingForJob && io.cmd.valid) {
    io.cmd.ready := Bool(true)
    dataBuffer := io.cmd.data
    state := State.start
  }

  io.txd := RegNext(txd, Bool(true))

}

