package spinal.lib.experimental.com.serial

import spinal.core._
import spinal.lib._

object SerialLinkConst {
  def cData = B"d01"
  def cClose = B"d02"
  def cOpen = B"d03"
  def cIsClose = B"d04"
  def cIsOpen = B"d05"

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
  val eNewFrame, eMyPtr0, eMyPtr1, eMessagePtr0, eMessagePtr1, eData = newElement()
}

class SerialLinkTx(bufferSize: Int, burstSize: Int, resendTimeoutLimit: Int) extends Component {
  assert(isPow2(bufferSize))

  import SerialLinkConst._
  import SerialLinkTxState._

  val io = new Bundle {
    val input = slave Stream (Bits(bitsWidth bit))
    val output = master Stream Fragment(Bits(bitsWidth bit))

    val rxToTx = in(new SerialLinkRxToTx)
  }


  //Timeouts
  val resendTimeout = Timeout(resendTimeoutLimit)
  val aliveTimeout = Timeout(resendTimeoutLimit)

  //tx buffer management
  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth bit), bufferSize)

    //Ring/Fifo ptr
    val writePtr = Counter(1 << 17)
    val readPtr = Counter(1 << 17)
    val syncPtr = Reg(UInt(17 bit))  //Last RX ack from the remote controller

    when(syncPtr === writePtr){
      resendTimeout.clear()
    }

    //Capacity flags
    val empty = writePtr(log2Up(bufferSize) downto  0) === readPtr(log2Up(bufferSize) downto  0)
    val full = writePtr(log2Up(bufferSize) downto  0) === (readPtr(log2Up(bufferSize) downto  0) ^ bufferSize)

    //Fill the buffer from upper layer data
    io.input.ready := !full
    when(io.input.fire) {
      ram.write(writePtr.resized, io.input.payload)
      writePtr.increment()
    }

    //manage syncPtr from rxToTx notification port
    when(io.rxToTx.otherRxPtr.fire) {
      when(syncPtr =/= io.rxToTx.otherRxPtr.payload) {
        resendTimeout.clear()
      }
      syncPtr := io.rxToTx.otherRxPtr.payload.resized
    }

    //read managment
    val readFlag = False
    val readPort = ram.readSync(readPtr.resized,readFlag)
    readPtr.willIncrement := readFlag

  }

  //Close/Open TX flags
  val sendClosingNotification = RegInit(True)
  val sendOpeningNotification = RegInit(False)
  sendClosingNotification := io.rxToTx.close | io.rxToTx.close
  sendOpeningNotification := io.rxToTx.open | io.rxToTx.open


  val otherWindow = 32 //TODO

  //Main state machine, it handle the protocole
  val statemachine = new Area {
    val state = RegInit(eNewFrame)
    val dataBuffer = Reg(Bits(8 bit))
    val txDataLeft = Reg(UInt(log2Up(burstSize+1) bit))
    val txDataLeftIsZero = txDataLeft === 0
    val isLastData = Reg(Bool)
    val isOpen = RegInit(False)

    //State before the connection opening
    when(!isOpen) {
      buffer.writePtr.valueNext := 0
      buffer.readPtr.valueNext := 0
      buffer.syncPtr := 0
    }

    io.output.valid := False
    io.output.last := False
    io.output.fragment := buffer.readPort

    when(buffer.readFlag){
      txDataLeft := txDataLeft - 1
    }

    //Statemachine switch case
    switch(state) {
      is(eNewFrame) {
        when(resendTimeout) {
          buffer.readPtr.valueNext := buffer.syncPtr
          resendTimeout.clear()
        }elsewhen(sendClosingNotification) {
          io.output.valid := True
          io.output.last := True
          io.output.fragment := cIsClose
          isOpen := False
          when(io.output.ready) {
            sendClosingNotification := False
          }
        }elsewhen(sendOpeningNotification) {
          io.output.valid := True
          io.output.last := True
          io.output.fragment := cIsOpen
          isOpen := True
          when(io.output.ready) {
            sendOpeningNotification := False
          }
        }elsewhen(isOpen && (!buffer.empty || aliveTimeout)) {
          io.output.valid := True
          io.output.fragment := cData
          when(io.output.ready) {
            state := eMyPtr0
            val otherRxBufferFreeSpace = otherWindow - (buffer.readPtr - buffer.syncPtr)
            txDataLeft := Mux[UInt](burstSize <  otherRxBufferFreeSpace,burstSize,burstSize).resized
          }
        }
      }
      is(eMyPtr0) {
        io.output.valid := True
        io.output.fragment := B(io.rxToTx.rxPtr(7 downto  0))
        dataBuffer := B(io.rxToTx.rxPtr(15 downto  8))
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
        io.output.fragment := B(buffer.readPtr(7 downto  0))
        when(io.output.ready) {
          state := eMessagePtr1
        }
      }
      is(eMessagePtr1) {
        io.output.valid := True
        io.output.fragment := B(buffer.readPtr(15 downto  8))

        when(buffer.empty || txDataLeftIsZero) {
          io.output.last := True
          when(io.output.ready) {
            state := eNewFrame
          }
        } otherwise {
          when(io.output.ready) {
            state := eData
            buffer.readFlag := True
          }
        }
        isLastData := False
      }
      is(eData) {
        io.output.valid := True
        when(!txDataLeftIsZero && !buffer.empty && !isLastData) {
          buffer.readFlag := io.output.ready
        } otherwise {
          isLastData := True
          io.output.last := True
          io.output.fragment := buffer.readPort
          when(io.output.ready) {
            state := eNewFrame
            resendTimeout.clear()
            aliveTimeout.clear()
          }
        }
      }
    }
  }
}


object SerialLinkRxState extends SpinalEnum {
  val eType, eOtherPtr0, eOtherPtr1, eMessagePtr0, eMessagePtr1, eData = newElement()
}


class SerialLinkRx extends Component {

  import SerialLinkConst._
  import SerialLinkRxState._

  val io = new Bundle {
    val input = slave Stream Fragment(Bits(bitsWidth bit))
    val output = master Stream (Bits(bitsWidth bit))

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
  io.output.payload := io.input.fragment
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
        io.rxToTx.otherRxPtr.push(U(data ## dataOld))
        state := eMessagePtr0
      }
      is(eMessagePtr0) {
        when(rxPtr(7 downto 0) =/= U(data)) {
          keepData := False
          io.rxToTx.miss := True
        }
        state := eMessagePtr1
        io.input.ready := True
      }
      is(eMessagePtr1) {
        when(rxPtr(15 downto 8) =/= U(data)) {
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
