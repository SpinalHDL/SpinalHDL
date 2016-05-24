package spinal.lib.com.uart

import spinal.core._
import spinal.lib.com.uart.UartStopType._
import spinal.lib.slave

/**
 * Created by PIC32F_USER on 24/05/2016.
 */


object UartCtrlTxState extends SpinalEnum {
  val sIdle,sStart, sData, sParity, sStop = newElement()
}

class UartCtrlTx(g : UartCtrlGenerics) extends Component {
  import g._

  val io = new Bundle {
    val configFrame = in(UartCtrlFrameConfig(g))
    val samplingTick = in Bool
    val write = slave Stream (Bits(dataWidthMax bit))
    val txd = out Bool
  }

  // Provide one tick each rxSamplePerBit io.samplingTick
  val clockDivider = new Area {
    val counter = Reg(UInt(log2Up(rxSamplePerBit) bits)) init(0)
    val tick = False
    when(io.samplingTick) {
      counter := counter - 1
      tick := counter === 0
    }
  }

  //Count up each tick
  val tickCounter = new Area {
    val value = Reg(UInt(Math.max(dataWidthMax, 2) bit))
    val reset = False

    when(clockDivider.tick) {
      value := value + 1
    }
    when(reset) {
      value := 0
    }
  }

  val stateMachine = new Area {
    import UartCtrlTxState._

    val state = RegInit(sIdle())
    val parity = Reg(Bool)
    val txd = True

    when(clockDivider.tick) {
      parity := parity ^ txd
    }

    io.write.ready := False
    switch(state) {
      is(sIdle){
        when(io.write.valid && clockDivider.tick){
          state := sStart
        }
      }
      is(sStart) {
        txd := False
        when(clockDivider.tick) {
          state := sData
          parity := io.configFrame.parity === UartParityType.eParityOdd
          tickCounter.reset := True
        }
      }
      is(sData) {
        txd := io.write.payload(tickCounter.value)
        when(clockDivider.tick) {
          when(tickCounter.value === io.configFrame.dataLength) {
            io.write.ready := True
            tickCounter.reset := True
            when(io.configFrame.parity === UartParityType.eParityNone) {
              state := sStop
            } otherwise {
              state := sParity
            }
          }
        }
      }
      is(sParity) {
        txd := parity
        when(clockDivider.tick) {
          state := sStop
          tickCounter.reset := True
        }
      }
      is(sStop) {
        when(clockDivider.tick) {
          when(tickCounter.value === toBitCount(io.configFrame.stop)) {
            state := io.write.valid ? sStart | sIdle
          }
        }
      }
    }
  }

  io.txd := RegNext(stateMachine.txd, True)
}