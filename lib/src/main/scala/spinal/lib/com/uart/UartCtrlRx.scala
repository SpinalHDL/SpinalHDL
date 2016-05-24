package spinal.lib.com.uart

import spinal.core._
import spinal.lib.com.uart.UartStopType._
import spinal.lib.{MajorityVote, BufferCC, master}

/**
 * Created by PIC32F_USER on 24/05/2016.
 */

object UartCtrlRxState extends SpinalEnum {
  val sIdle, sStart, sData, sParity, sStop = newElement()
}

class UartCtrlRx(g : UartCtrlGenerics) extends Component {
  import g._
  val io = new Bundle {
    val configFrame = in(UartCtrlFrameConfig(g))
    val samplingTick = in Bool
    val read = master Flow (Bits(dataWidthMax bit))
    val rxd = in Bool
  }

  //Implement the rx sampling with a majority vote over samplingSize bits
  val sampler = new Area {
    val frontBuffer = BufferCC(io.rxd)
    val samples = RegInit(BitsSet(samplingSize bit))
    when(io.samplingTick) {
      samples := (samples ## frontBuffer).resized
    }
    val value = RegNext(MajorityVote(samples))
    val event = RegNext(io.samplingTick)
  }

  //Provide a tick each rxSamplePerBit
  val bitTimer = new Area {
    val counter = Reg(UInt(log2Up(rxSamplePerBit) bit))
    def reset = counter := U(preSamplingSize + (samplingSize - 1) / 2 - 1)
    val tick = Bool

    tick := False
    when(sampler.event) {
      counter := counter - 1
      when(counter === 0) {
        tick := True
      }
    }


  }

  //Count bitTimer.tick
  val bitCounter = new Area {
    val value = Reg(UInt(Math.max(dataWidthMax, 2) bit))
    def reset = value := 0

    when(bitTimer.tick) {
      value := value + 1
    }
  }

  val stateMachine = new Area {
    import UartCtrlRxState._

    val state = RegInit(sIdle())
    val parity = Reg(Bool)
    val shifter = Reg(io.read.payload)

    when(bitTimer.tick) {
      parity := parity ^ sampler.value
    }

    io.read.valid := False
    switch(state) {
      is(sIdle) {
        when(sampler.value === False) {
          state := sStart
          bitTimer.reset
        }
      }
      is(sStart) {
        when(bitTimer.tick) {
          state := sData
          bitCounter.reset
          parity := io.configFrame.parity === UartParityType.eParityOdd
          when(sampler.value === True) {
            state := sIdle
          }
        }
      }
      is(sData) {
        when(bitTimer.tick) {
          shifter(bitCounter.value) := sampler.value
          when(bitCounter.value === io.configFrame.dataLength) {
            bitCounter.reset
            when(io.configFrame.parity === UartParityType.eParityNone) {
              state := sStop
            } otherwise {
              state := sParity
            }
          }
        }
      }
      is(sParity) {
        when(bitTimer.tick) {
          state := sStop
          bitCounter.reset
          when(parity =/= sampler.value) {
            state := sIdle
          }
        }
      }
      is(sStop) {
        when(bitTimer.tick) {
          when(!sampler.value) {
            state := sIdle
          }.elsewhen(bitCounter.value === toBitCount(io.configFrame.stop)) {
            state := sIdle
            io.read.valid := True
          }
        }
      }
    }
  }
  io.read.payload := stateMachine.shifter
}