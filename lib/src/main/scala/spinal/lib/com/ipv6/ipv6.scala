package spinal.lib.com.ipv6

import spinal.core._
import spinal.lib._


//https://en.wikipedia.org/wiki/IPv6_packet
object Ipv6RxState extends SpinalEnum {
  val eFilter, eHeader, eData = Value
}

class Ipv6Rx extends Component {
  val io = new Bundle {
    val ip = in Vec(Bits(8 bit),16)

    // IPV6
    val inFrame = slave Stream Fragment(Bits(8 bit))

    // Next Header / PayloadLength / src address / Data
    val outFrame = master Stream Fragment(Bits(8 bit))
  }


  val headerBuffer = new Area {
    def size = 1 + 2 + 16

    //Control signals
    val readPtr = Counter(log2Up(size))
    val writePtr = U(0, log2Up(size) bit)
    val writeFlag = False
    val readFlag = False

    //Memory and his control
    val ram = Mem(Bits(8 bit), size)
    when(writeFlag) {
      ram(writePtr) := io.inFrame.fragment
    }

    val readPort = ram.readSync(readPtr, readFlag)
    readPtr.willIncrement := readFlag

    //User interface
    def writeAt(at: UInt): Unit = {
      writeFlag := True
      writePtr := at
    }

    def readIt: Unit = {
      readFlag := True
    }
  }


  val dataStream = io.inFrame //Stream Fragment(Bits(8 bit))

  val statemachine = new Area {
    io.inFrame.ready := False
    io.outFrame.valid := False
    io.outFrame.data := io.inFrame.data

    import Ipv6RxState._

    val state = RegInit(eFilter)
    val headerByteCounter = Reg(UInt(6 bit)) init (0)
    val keepPacket = RegInit(True)
    val noPayload = RegInit(True)
    val resetStateMachine = False

    switch(state) {
      is(eFilter) {
        io.inFrame.ready := True
        when(io.inFrame.valid) {
          when(headerByteCounter === 4)(headerBuffer.writeAt(1))
          when(headerByteCounter === 5)(headerBuffer.writeAt(2))
          when(headerByteCounter === 6)(headerBuffer.writeAt(0))
          when(headerByteCounter >= 8 && headerByteCounter < 24)(headerBuffer.writeAt(headerByteCounter - 8 + 3))

          val keepPacketNext = keepPacket
          when(headerByteCounter === 0 && ((io.inFrame.fragment >> 4) !== 0x6)
            || headerByteCounter >= 24 && (io.inFrame.fragment !== io.ip(headerByteCounter(3, 0) ^ 8))) {
            keepPacketNext := False
          }
          keepPacket := keepPacketNext

          when(headerByteCounter === 39 && keepPacketNext) {
            state := eData
            headerBuffer.readIt
            noPayload := io.inFrame.last
          } otherwise {
            resetStateMachine := True
          }
        }
      }
      is(eHeader) {
        io.outFrame.valid := True
        io.outFrame.fragment := headerBuffer.readPort
        when(io.outFrame.ready) {
          when(headerBuffer.readPtr !== 0) {
            headerBuffer.readIt
          }.elsewhen(noPayload) {
            io.outFrame.last := True
          }.otherwise {
            state := eData
          }
        }
      }
      is(eData) {
        io.outFrame << dataStream
      }
    }


    when(resetStateMachine || io.outFrame.fire && io.outFrame.last) {
      state := eFilter
      headerByteCounter := 0
      keepPacket := False
    }
  }
}

object Ipv6TxState extends SpinalEnum {
  val ePopNextHeader, eHeader, eData = Value
}

class Ipv6Tx extends Component {
  val io = new Bundle {
    val ip = in Vec(Bits(8 bit),16)

    // Next Header / PayloadLength / dst address / Data
    val inFrame = slave Stream Fragment(Bits(8 bit))

    // IPV6
    val outFrame = master Stream Fragment(Bits(8 bit))
  }

  io.inFrame.ready := False
  io.outFrame.valid := False
  io.outFrame.last := False
  io.outFrame.fragment := io.inFrame.fragment

  import Ipv6TxState._

  val state = RegInit(ePopNextHeader)
  val headerByteCounter = Reg(UInt(6 bit)) init (0)
  val nextHeader = Reg(Bits(8 bit))

  switch(state) {
    is(ePopNextHeader) {
      io.inFrame.ready := True
      when(io.inFrame.valid) {
        nextHeader := io.inFrame.fragment
        state := eHeader
      }
    }
    is(eHeader) {
      switch(headerByteCounter) {
        is(0) {
          io.outFrame.valid := True
          io.outFrame.fragment := 0x60
        }
        is(1, 2, 3) {
          io.outFrame.valid := True
          io.outFrame.fragment := 0x00
        }
        is(7) {
          io.outFrame.valid := True
          io.outFrame.fragment := 0x80
        }
        is(6) {
          io.outFrame.valid := True
          io.outFrame.fragment := nextHeader
        }
        is(8 to 23) {
          io.outFrame.valid := True
          io.outFrame.fragment := io.ip(headerByteCounter - 8)
        }
        default {
          io.outFrame << io.inFrame
        }
      }
      when(headerByteCounter === 39 && io.outFrame.fire) {
        state := eData
      }
    }
    is(eData) {
      io.outFrame << io.inFrame
    }
  }

  when(io.outFrame.fire) {
    headerByteCounter := headerByteCounter + 1
    when(io.outFrame.last) {
      state := ePopNextHeader
      headerByteCounter := 0
    }
  }
}