package spinal.lib.serdes

import spinal.core._
import spinal.lib._

object SerialCheckerConst {
  def cMagic = B"xA5"
  def cStart = B"xD8"
  def cEnd = B"x9A"

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

  import SerialCheckerTxState._

  val io = new Bundle {
    val input = slave Stream Fragment(Bits(bitsWidth bit))
    val output = master Stream (new SerialCheckerPhysical(bitsWidth))
  }

  io.output.valid := False
  io.output.data.bits := io.input.fragment
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
        checksum := 0
      }
      is(eData) {
        io.output.valid := io.input.valid
        io.output.data.bits := io.input.fragment
        io.input.ready := io.output.ready
        when(io.output.fire) {
          checksum := checksum + toUInt(io.input.fragment)
          when(io.input.last) {
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
        io.output.data.bits := toBits(checksum(7, 0))
        when(io.output.ready) {
          state := eCheck1
        }
      }
      is(eCheck1) {
        io.output.valid := True
        io.output.data.bits := toBits(checksum(15, 8))
        when(io.output.ready) {
          state := eStart
        }
      }
    }
  }
}


object SerialCheckerRxState extends SpinalEnum {
  val eIdle, eData, eCheck0, eCheck1 = Value
}


class SerialCheckerPhysicalToSerial(bitsWidth: Int) extends Component {

  import SerialCheckerConst._

  val io = new Bundle {
    val input = slave Stream (new SerialCheckerPhysical(bitsWidth))
    val output = master Stream (Bits(bitsWidth bit))
  }

  val inMagic = RegInit(False)

  io.output.valid := io.input.valid
  io.output.data := io.input.data.bits
  io.input.ready := io.output.ready

  when(inMagic) {
    when(io.input.data.isStart) {
      io.output.data := cStart
    }
    when(io.input.data.isEnd) {
      io.output.data := cEnd
    }
    when(io.output.fire) {
      inMagic := False
    }
  } otherwise {
    when(!io.input.data.isBits || io.input.data.bits === cMagic) {
      io.input.ready := False
      io.output.data := cMagic
      when(io.output.fire) {
        inMagic := True
      }
    }
  }
}

class SerialCheckerPhysicalfromSerial(bitsWidth: Int) extends Component {

  import SerialCheckerConst._

  val io = new Bundle {
    val input = slave Flow (Bits(bitsWidth bit))
    val output = master Flow (new SerialCheckerPhysical(bitsWidth))
  }

  val inMagic = RegInit(False)

  io.output.valid := False
  io.output.data.bits := io.input.data
  io.output.data.isStart := False
  io.output.data.isEnd := False

  when(io.input.fire) {
    when(inMagic) {
      switch(io.input.data) {
        is(cStart) {
          io.output.valid := True
          io.output.data.isStart := True
        }
        is(cEnd) {
          io.output.valid := True
          io.output.data.isEnd := True
        }
        is(cMagic) {
          io.output.valid := True
        }
      }
      inMagic := False
    } otherwise {
      when(io.input.data === cMagic) {
        inMagic := True
      } otherwise {
        io.output.valid := True
      }
    }
  }
}


class SerialCheckerRx(wordCountMax: Int) extends Component {
  assert(isPow2(wordCountMax))

  import SerialCheckerConst._
  import SerialCheckerRxState._

  val io = new Bundle {
    val input = slave Flow (new SerialCheckerPhysical(bitsWidth))
    val output = master Stream Fragment(Bits(bitsWidth bit))
  }

  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth + 1 bit), wordCountMax)
    val writePtr = Counter(wordCountMax << 1)
    val validPtr = Reg(UInt(log2Up(wordCountMax) bit)) init (0)

    val checksum = Reg(UInt(16 bit))

    val pushFlag = False
    val flushFlag = False

    val lastWriteData = RegNextWhen(io.input.data.bits(bitsWidth - 1, 0), pushFlag)

    when(pushFlag || flushFlag) {
      ram(writePtr - toUInt(flushFlag)) := Mux(pushFlag, io.input.data.bits, True ## lastWriteData)
    }

    when(pushFlag) {
      checksum := checksum + toUInt(io.input.data.bits) //TODO better checksum
      writePtr.increment()
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
      writePtr.valueNext := validPtr
      checksum := 0
    }


    val readPtr = Counter(wordCountMax)
    val readCmd = Stream(UInt(log2Up(wordCountMax) bit))
    readCmd.valid := (validPtr =/= readPtr)
    readCmd.data := readPtr
    readPtr.willIncrement := readCmd.fire
    io.output.translateFrom(ram.streamReadSync(readCmd))((to, from) => {
      to.last := from.msb
      to.fragment := from
    })

  }


  val stateMachine = new Area {
    val state = RegInit(eIdle)
    val overflow = Reg(Bool)
    when(io.input.fire) {
      when(io.input.data.isBits) {
        switch(state) {
          is(eData) {
            overflow := overflow | !buffer.push
          }
          is(eCheck0) {
            when(io.input.data.bits === toBits(buffer.checksum(7, 0))) {
              state := eCheck1
            } otherwise {
              state := eIdle
            }
          }
          is(eCheck1) {
            when(!overflow && io.input.data.bits === toBits(buffer.checksum(15, 8))) {
              buffer.flush
            }
            state := eIdle
          }
        }
      }
      when(io.input.data.isStart) {
        state := eData
        overflow := False
        buffer.writeStart
      }
      when(io.input.data.isEnd) {
        state := eCheck0
      }
    }


  }
}
