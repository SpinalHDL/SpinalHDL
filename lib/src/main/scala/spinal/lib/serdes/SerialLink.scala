package spinal.lib.serdes

import spinal.core._
import spinal.lib._

object SerialLinkConst {
  def cData = B"01"

  def cClose = B"02"

  def cOpen = B"03"

  def cIsClose = B"04"

  def cIsOpen = B"05"

  def chunkDataSizeMax = 32

  def bitsWidth = 8
}

class SerialLinkRxToTx extends Bundle {
  val close = Bool
  val open = Bool
  val miss = Bool
  val rxPtr = UInt(16 bit)
  val otherRxPtr = Flow(UInt(16 bit))
}

object SerialLinkTxState extends SpinalEnum {
  val eNewFrame, eMyPtr0, eMyPtr1, eMessagePtr0, eMessagePtr1, eData = Value
}

class SerialLinkTx(bufferSize: Int, burstSize: Int, resendTimeoutLimit: Int) extends Component {
  assert(isPow2(bufferSize))

  import SerialLinkConst._
  import SerialLinkTxState._

  val io = new Bundle {
    val input = slave Handshake (Bits(bitsWidth bit))
    val output = master Handshake Fragment(Bits(bitsWidth bit))

    val rxToTx = in(new SerialLinkRxToTx)
  }

  val resend = new Area {
    //TODO maybe add to Utils lib the timeout pattern ?
    val value = RegInit(False)

    val timeout = CounterFreeRun(resendTimeoutLimit)
    when(timeout.overflow) {
      value := True
    }

    def clear: Unit = {
      timeout.reset
      value := False
    }
  }

  val alive = new Area {
    val value = RegInit(False)

    val timeout = CounterFreeRun(resendTimeoutLimit)
    when(timeout.overflow) {
      value := True
    }

    when(io.output.fire) {
      timeout.reset
      value := False
    }
  }


  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth bit), bufferSize)

    val writePtr = Counter(1 << 16)
    val readPtr = Counter(1 << 16)
    val syncPtr = Reg(UInt(16 bit))


    val empty = writePtr(log2Up(bufferSize), 0) === readPtr(log2Up(bufferSize), 0)
    val full = writePtr(log2Up(bufferSize), 0) === (readPtr(log2Up(bufferSize), 0) ^ bufferSize)

    io.input.ready := !full
    when(io.input.fire) {
      ram.write(writePtr, io.input.data)
      writePtr ++
    }

    when(io.rxToTx.otherRxPtr.fire) {
      syncPtr := io.rxToTx.otherRxPtr.data
      when(syncPtr !== io.rxToTx.otherRxPtr.data) {
        resend.clear
      }
    }

    val readCmdFlag = False
    //No overflow protection !
    val readCmd = Handshake(UInt(log2Up(bufferSize) bit))
    readCmd.valid := readCmdFlag
    readCmd.data := readPtr
    when(readCmd.fire) {
      readPtr ++
    }
    val readPort = ram.handshakeReadSync(readCmd)

  }

  val close = RegInit(True)
  val open = RegInit(False)
  when(io.rxToTx.close) {
    close := True
  }
  when(io.rxToTx.open) {
    open := True
  }


  val statemachine = new Area {
    val state = RegInit(eNewFrame)
    val dataBuffer = Reg(Bits(8 bit))
    val dataCounter = Reg(UInt(log2Up(burstSize) bit))
    val isLastData = Reg(Bool)
    val isOpen = RegInit(False)

    when(buffer.readCmd.fire) {
      dataCounter := dataCounter + 1
    }
    when(!isOpen) {
      buffer.writePtr := 0
      buffer.readPtr := 0
      buffer.syncPtr := 0
    }

    io.output.valid := False
    io.output.last := False
    io.output.fragment := buffer.readPort.data
    buffer.readPort.ready := io.output.ready

    switch(state) {
      is(eNewFrame) {
        when(resend.value) {
          buffer.readPtr := buffer.syncPtr
          resend.clear
        }.elsewhen(close) {
          io.output.valid := True
          io.output.last := True
          io.output.fragment := cIsClose
          isOpen := False
          when(io.output.ready) {
            close := False
          }
        }.elsewhen(open) {
          io.output.valid := True
          io.output.last := True
          io.output.fragment := cIsOpen
          isOpen := True
          when(io.output.ready) {
            open := False
          }
        }.elsewhen(isOpen && (!buffer.empty || alive.value)) {
          io.output.valid := True
          io.output.fragment := cData
          when(io.output.ready) {
            state := eMyPtr0
            dataCounter := 0
          }
        }
      }
      is(eMyPtr0) {
        io.output.valid := True
        io.output.fragment := toBits(io.rxToTx.rxPtr(7, 0))
        dataBuffer := toBits(io.rxToTx.rxPtr(15, 8))
        when(io.output.ready) {
          state := eMyPtr1
        }
      }
      is(eMyPtr1) {
        io.output.valid := True
        io.output.fragment := dataBuffer
        when(buffer.empty) {
          io.output.last := True
          when(io.output.ready) {
            state := eNewFrame
          }
        } otherwise {
          when(io.output.ready) {
            state := eMessagePtr0
          }
        }
      }
      is(eMessagePtr0) {
        io.output.valid := True
        io.output.fragment := toBits(buffer.readPtr(7, 0))
        when(io.output.ready) {
          state := eMessagePtr1
        }
      }
      is(eMessagePtr1) {
        io.output.valid := True
        io.output.fragment := toBits(buffer.readPtr(15, 8))

        when(buffer.empty) {
          io.output.last := True
          when(io.output.ready) {
            state := eNewFrame
          }
        } otherwise {
          buffer.readCmdFlag := True
          when(io.output.ready) {
            state := eData
          }
        }
        isLastData := False
      }
      is(eData) {
        io.output.valid := True
        when((dataCounter !== burstSize - 1) && !buffer.empty && !isLastData) {
          buffer.readCmdFlag := True
        } otherwise {
          isLastData := True
          io.output.last := True
          when(io.output.ready) {
            state := eNewFrame
          }
        }
      }
    }
  }
}


object SerialLinkRxState extends SpinalEnum {
  val eType, eOtherPtr0, eOtherPtr1, eMessagePtr0, eMessagePtr1, eData = Value
}


class SerialLinkRx extends Component {

  import SerialLinkConst._
  import SerialLinkRxState._

  val io = new Bundle {
    val input = slave Handshake Fragment(Bits(bitsWidth bit))
    val output = master Handshake (Bits(bitsWidth bit))

    val rxToTx = out(new SerialLinkRxToTx)
  }

  val softReset = RegInit(True)
  val state = RegInit(eType)
  val rxPtr = Reg(UInt(16 bit))
  val keepData = RegInit(False)

  val data = io.input.fragment
  val dataOld = RegNextWhen(data, io.input.valid)

  io.rxToTx.close := False
  io.rxToTx.open := False
  io.rxToTx.miss := False
  io.rxToTx.otherRxPtr.default(0)

  io.output.valid := False
  io.output.data := io.input.fragment
  io.input.ready := False
  when(io.input.valid) {
    switch(state) {
      is(eType) {
        io.input.ready := True
        switch(data) {
          is(cClose) {
            softReset := True
            io.rxToTx.close := True
          }
          is(cOpen) {
            softReset := False
            rxPtr := 0
            io.rxToTx.otherRxPtr.push(0)
            io.rxToTx.open := True
          }
          is(cData) {
            state := eOtherPtr0
            keepData := True
          }
        }
      }
      is(eOtherPtr0) {
        io.input.ready := True
        state := eOtherPtr1
      }
      is(eOtherPtr1) {
        io.input.ready := True
        io.rxToTx.otherRxPtr.push(toUInt(data ## dataOld))
        state := eMessagePtr0
      }
      is(eMessagePtr0) {
        when(rxPtr(7, 0) !== toUInt(data)) {
          keepData := False
          io.rxToTx.miss := True
        }
        state := eMessagePtr1
        io.input.ready := True
      }
      is(eMessagePtr1) {
        when(rxPtr(15, 8) !== toUInt(data)) {
          keepData := False
          io.rxToTx.miss := keepData
        }
        state := eData
        io.input.ready := True
      }
      is(eData) {
        io.output.valid := io.input.valid && keepData
        io.input.ready := io.output.ready
      }
    }

  }

  when(io.output.fire) {
    rxPtr := rxPtr + 1
  }
  when(io.input.isLast && io.input.ready) {
    state := eType
  }

  io.rxToTx.rxPtr := rxPtr
}
