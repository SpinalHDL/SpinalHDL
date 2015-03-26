package spinal.lib.serdes

import spinal.core._
import spinal.lib._

object SerialCheckerConst {
  //  def cMagic = b"xA5"
  //  def cStart = b"xD8"
  //  def cEnd = b"x9A"

  def chunkDataSizeMax = 32
  def bitsWidth = 8
}

object SerialCheckerTxState extends SpinalEnum {
  val eStart, eData, eEnd, eCheck0, eCheck1 = Value()
}


class SerialCheckerPhysical(bitsWidth: Int) extends Bundle {
  val bits = Bits(bitsWidth bit)
  val isStart = Bool
  val isEnd = Bool

  def isBits = !isStart && !isEnd

  override def clone(): this.type = new SerialCheckerPhysical(bitsWidth).asInstanceOf[this.type]
}

class SerialCheckerTx(bitsWidth: Int) extends Component {

  import SerialCheckerConst._
  import SerialCheckerTxState._

  val io = new Bundle {
    val input = slave Handshake Fragment(Bits(bitsWidth bit))
    val output = master Handshake (new SerialCheckerPhysical(bitsWidth))
  }

  io.output.valid := False
  io.output.data.bits := io.input.data.fragment
  io.output.data.isBits := False
  io.output.data.isStart := False
  io.output.data.isEnd := False
  io.input.ready := False

  val stateMachine = new Area {
    val state = RegInit(eStart)
    val lookingForJob = False
    val checksum = Reg(UInt(16 bit))
    switch(state) {
      is(eStart) {
        when(io.input.valid) {
          io.output.valid := True
          io.output.data.isStart := True
          when(io.output.ready) {
            state := eData
          }
        }
      }
      is(eData) {
        io.output.valid := io.input.valid
        io.output.data.bits := io.input.data.fragment
        io.output.data.isBits := True
        when(!io.input.valid) {
          state := eEnd
        }
        when(io.output.fire) {
          checksum := checksum + toUInt(io.input.data.fragment)
          io.input.ready := True
          when(io.input.data.last) {
            state := eEnd
          }
        }
      }
      is(eEnd) {
        io.output.valid := True
        io.output.data.isEnd := True
        when(io.output.ready) {
          state := eCheck0
        }
      }
      is(eCheck0) {
        io.output.valid := True
        io.output.data := checksum(7, 0)
        io.output.data.isBits := True
        when(io.output.ready) {
          state := eCheck1
        }
      }
      is(eCheck1) {
        io.output.valid := True
        io.output.data := checksum(15, 8)
        io.output.data.isBits := True
        when(io.output.ready) {
          state := eStart
        }
      }
    }
  }
}


object SerialCheckerRxState extends SpinalEnum {
  val idle, data, check0, check1 = Value
}


//class MagicRx(bitsWidth: Int) extends Component{
//  val io = new Bundle{
//    val input = slave Flow (new SerialCheckerPhysical(bitsWidth))
//    val output = master Flow (new SerialCheckerPhysical(bitsWidth))
//  }
//}
//val state = RegInit(idle)
//val inMagic = RegInit(False)
//val consumeData = False
//val overflow = Reg(Bool)
//when(io.input.fire) {
//when(inMagic) {
//switch(io.input.data) {
//is(cStart) {
//state := data
//overflow := False
//buffer.writeStart
//}
//is(cEnd) {
//state := check0
//}
//is(cMagic) {
//consumeData := True
//}
//default {
//state := idle
//}
//}
//} otherwise {
//when(io.input.data === cMagic) {
//inMagic := True
//} otherwise {
//consumeData := True
//}
//}
//}

class SerialCheckerRx(wordCountMax: Int) extends Component {
  assert(isPow2(wordCountMax))

  import SerialCheckerConst._
  import SerialCheckerRxState._

  val io = new Bundle {
    val input = slave Flow (new SerialCheckerPhysical(bitsWidth))
    val output = master Handshake Fragment(Bits(bitsWidth bit))
  }

  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth + 1 bit), wordCountMax)
    val writePtr = Counter(wordCountMax << 1)
    val validPtr = Reg(UInt(log2Up(wordCountMax) bit)) init (0)

    val checksum = Reg(UInt(16 bit))

    val pushFlag = False
    val flushFlag = False

    val lastWriteData = RegNextWhen(io.input.data.bits, pushFlag)

    when(pushFlag || flushFlag) {
      ram(writePtr - toUInt(flushFlag)) := Mux(pushFlag, io.input.data.bits, True ## lastWriteData)
    }

    when(pushFlag) {
      checksum := checksum + toUInt(io.input.data.bits) //TODO better checksum
      writePtr ++
    }
    when(flushFlag) {
      validPtr := writePtr
    }

    def push: Bool = {
      val success = !(writePtr === (readPtr ^ wordCountMax))
      when(success) {
        pushFlag := True
      }
      success
    }

    def flush: Unit = {
      flushFlag := True
    }
    def writeStart: Unit = {
      writePtr := validPtr
      checksum := 0
    }


    val readPtr = Counter(wordCountMax)
    val readCmd = Handshake(UInt(log2Up(wordCountMax) bit))
    readCmd.valid := (validPtr !== readPtr)
    readCmd.data := readPtr
    readPtr.inc := readCmd.fire
    io.output.connectFrom(ram.handshakeReadSync(readCmd))((to, from) => {
      to.last := from.msb
      to.fragment := from
    })

  }


  val stateMachine = new Area {
    val state = RegInit(idle)
    val overflow = Reg(Bool)
    when(io.input.fire) {
      when(io.input.data.isBits) {
        switch(state) {
          is(data) {
            overflow := overflow | !buffer.push
          }
          is(check0) {
            when(io.input.data === toBits(buffer.checksum(7, 0))) {
              state := check1
            } otherwise {
              state := idle
            }
          }
          is(check1) {
            when(!overflow && io.input.data === toBits(buffer.checksum(15, 8))) {
              buffer.flush
            }
            state := idle
          }
        }
      }
      when(io.input.data.isStart) {
        state := data
        overflow := False
        buffer.writeStart
      }
      when(io.input.data.isEnd) {
        state := check0
      }
    }


  }
}
