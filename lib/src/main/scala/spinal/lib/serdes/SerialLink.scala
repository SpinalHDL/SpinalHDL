package spinal.lib.serdes

import spinal.core._
import spinal.lib._

object SerialLinkConst {
  def cData = b"01"
  def cClose = b"02"
  def cOpen = b"03"

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
    val input = slave Handshake Fragment(Bits(bitsWidth bit))
    val output = master Handshake Fragment(Bits(bitsWidth bit))

    val rxToTx = in(new SerialLinkRxToTx)
  }

  val resend = new Area {
    //TODO maybe add to Utils lib the timeout pattern ?
    val value = RegInit(False)

    val timeout = Counter(resendTimeoutLimit)
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

    val timeout = Counter(resendTimeoutLimit)
    when(timeout.overflow) {
      value := True
    }

    when(io.output.valid){
      timeout.reset
      value := False
    }
  }


  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth bit), bufferSize)

    val writePtr = Counter(bufferSize << 1)
    val readPtr = Counter(bufferSize << 1)
    val syncPtr = UInt(log2Up(bufferSize) bit)


    val empty = writePtr === readPtr
    val full = writePtr === (readPtr ^ bufferSize)


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
  when(io.rxToTx.close){
    close := True
  }
  when(io.rxToTx.open){
    open := True
  }


  val statemachine = new Area {
    val state = RegInit(eNewFrame)
    val dataBuffer = Reg(Bits(8 bit))
    val dataCounter = Reg(UInt(log2Up(burstSize) bit))
    val isLastData = Reg(Bool)

    when(buffer.readCmd.fire) {
      dataCounter := dataCounter + 1
    }

    io.output.valid := False
    io.output.data := io.input.data
    io.input.ready := False
    buffer.readPort.ready := io.output.ready

    switch(state) {
      is(eNewFrame) {
        when(resend.value) {
          buffer.readPtr := buffer.syncPtr
          resend.clear
        }.elsewhen(close){
          io.output.valid := True
          io.output.data.last := True
          io.output.data.fragment := cClose
          when(io.output.ready){
            close := False
          }
        }.elsewhen(open){
          io.output.valid := True
          io.output.data.last := True
          io.output.data.fragment := cOpen
          when(io.output.ready){
            open := False
          }
        }.elsewhen(!buffer.empty || alive.value) {
          io.output.valid := True
          io.output.data.fragment := cData
          when(io.output.ready) {
            state := eMyPtr0
            dataCounter := 0
          }
        }
      }
      is(eMyPtr0) {
        io.output.valid := True
        io.output.data.fragment := io.rxToTx.rxPtr(7, 0)
        dataBuffer := io.rxToTx.rxPtr(15, 8)
        when(io.output.ready) {
          state := eMyPtr1
        }
      }
      is(eMyPtr1) {
        io.output.valid := True
        io.output.data.fragment := dataBuffer
        when(io.output.ready) {
          state := eMessagePtr0
        }
      }
      is(eMessagePtr0) {
        io.output.valid := True
        io.output.data.fragment := buffer.readPtr(7, 0)
        when(io.output.ready) {
          state := eMessagePtr1
        }
      }
      is(eMessagePtr1) {
        io.output.valid := True
        io.output.data.fragment := buffer.readPtr(15, 8)

        when(buffer.empty) {
          io.output.data.last := True
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
        io.output.data.fragment := buffer.readPort.data
        when((dataCounter !== burstSize - 1) && !buffer.empty && !isLastData) {
          buffer.readCmdFlag := True
        } otherwise {
          isLastData := True
          io.output.data.last := True
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
    val output = master Handshake Fragment(Bits(bitsWidth bit))

    val rxToTx = out(new SerialLinkRxToTx)
  }

  val softReset = RegInit(True)
  val state = RegInit(eType)
  val rxPtr = Reg(UInt(16 bit))
  val keepData = Reg(Bool)

  val data = io.input.data.fragment
  val dataOld = RegNextWhen(data, io.input.valid)

  io.rxToTx.close := False
  io.rxToTx.open := False
  io.rxToTx.miss := False

  io.output.valid := io.input.valid && keepData
  io.output.data := io.input.data
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
