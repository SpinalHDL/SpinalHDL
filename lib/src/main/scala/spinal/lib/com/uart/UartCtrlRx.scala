package spinal.lib.com.uart

import spinal.core._
import spinal.lib.com.uart.UartStopType._
import spinal.lib._

object UartCtrlRxState extends SpinalEnum {
  val IDLE, START, DATA, PARITY, STOP = newElement()
}

class UartCtrlRx(g : UartCtrlGenerics) extends Component {
  import g._
  val io = new Bundle {
    val configFrame  = in(UartCtrlFrameConfig(g))
    val samplingTick = in Bool
    val read         = master Flow (Bits(dataWidthMax bit))
    val rxd          = in Bool
  }

  // Implement the rxd sampling with a majority vote over samplingSize bits
  // Provide a new sampler.value each time sampler.tick is high
  val sampler = new Area {
    val syncroniser = BufferCC(io.rxd,init=False)
    val samples     = History(that=syncroniser,length=samplingSize,when=io.samplingTick,init=True)
    val value       = RegNext(MajorityVote(samples)) init(True)
    val tick        = RegNext(io.samplingTick) init(False)
  }

  // Provide a bitTimer.tick each rxSamplePerBit
  // reset() can be called to recenter the counter over a start bit.
  val bitTimer = new Area {
    val counter = Reg(UInt(log2Up(rxSamplePerBit) bit))
    def reset() = counter := preSamplingSize + (samplingSize - 1) / 2 - 1
    val tick = False
    when(sampler.tick) {
      counter := counter - 1
      when(counter === 0) {
        tick := True
        if(!isPow2(rxSamplePerBit))
          counter := rxSamplePerBit-1
      }
    }
  }

  // Provide bitCounter.value that count up each bitTimer.tick, Used by the state machine to count data bits and stop bits
  // reset() can be called to reset it to zero
  val bitCounter = new Area {
    val value = Reg(UInt(Math.max(log2Up(dataWidthMax), 2) bit))
    def reset() = value := 0

    when(bitTimer.tick) {
      value := value + 1
    }
  }

  val stateMachine = new Area {
    import UartCtrlRxState._

    val state   = RegInit(IDLE)
    val parity  = Reg(Bool)
    val shifter = Reg(io.read.payload)
    val validReg = RegNext(False) init(False)
    io.read.valid := validReg

    //Parity calculation
    when(bitTimer.tick) {
      parity := parity ^ sampler.value
    }



    switch(state) {
      is(IDLE) {
        when(sampler.tick && !sampler.value) {
          state := START
          bitTimer.reset()
        }
      }
      is(START) {
        when(bitTimer.tick) {
          state := DATA
          bitCounter.reset()
          parity := io.configFrame.parity === UartParityType.ODD
          when(sampler.value === True) {
            state := IDLE
          }
        }
      }
      is(DATA) {
        when(bitTimer.tick) {
          shifter(bitCounter.value) := sampler.value
          when(bitCounter.value === io.configFrame.dataLength) {
            bitCounter.reset()
            when(io.configFrame.parity === UartParityType.NONE) {
              state := STOP
              validReg := True
            } otherwise {
              state := PARITY
            }
          }
        }
      }
      is(PARITY) {
        when(bitTimer.tick) {
          bitCounter.reset()
          when(parity === sampler.value) {
            state := STOP
            validReg := True
          } otherwise {
            state := IDLE
          }
        }
      }
      is(STOP) {
        when(bitTimer.tick) {
          when(!sampler.value) {
            state := IDLE
          }.elsewhen(bitCounter.value === toBitCount(io.configFrame.stop)) {
            state := IDLE
          }
        }
      }
    }
  }
  io.read.payload := stateMachine.shifter
}