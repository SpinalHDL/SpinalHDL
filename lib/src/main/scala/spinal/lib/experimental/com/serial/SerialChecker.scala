package spinal.lib.experimental.com.serial

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
  val eStart, eData, eEnd, eCheck0, eCheck1 = newElement()
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
  io.output.bits := io.input.fragment
  io.output.isStart := False
  io.output.isEnd := False
  io.input.ready := False

  val stateMachine = new Area {
    val state = RegInit(eStart)
    val lookingForJob = False
    val checksum = Reg(UInt(16 bit))
    switch(state) {
      is(eStart) {
        when(io.input.valid) {
          io.output.valid := True
          io.output.isStart := True
          when(io.output.ready) {
            state := eData
          }
        }
        checksum := 0
      }
      is(eData) {
        io.output.valid := io.input.valid
        io.output.bits := io.input.fragment
        io.input.ready := io.output.ready
        when(io.output.fire) {
          checksum := checksum + U(io.input.fragment)
          when(io.input.last) {
            state := eEnd
          }
        }
      }
      is(eEnd) {
        io.output.valid := True
        io.output.isEnd := True
        when(io.output.ready) {
          state := eCheck0
        }
      }
      is(eCheck0) {
        io.output.valid := True
        io.output.bits := B(checksum(7 downto 0))
        when(io.output.ready) {
          state := eCheck1
        }
      }
      is(eCheck1) {
        io.output.valid := True
        io.output.bits := B(checksum(15 downto 8))
        when(io.output.ready) {
          state := eStart
        }
      }
    }
  }
}


object SerialCheckerRxState extends SpinalEnum {
  val eIdle, eData, eCheck0, eCheck1 = newElement()
}


class SerialCheckerPhysicalToSerial(bitsWidth: Int) extends Component {

  import SerialCheckerConst._

  val io = new Bundle {
    val input = slave Stream (new SerialCheckerPhysical(bitsWidth))
    val output = master Stream (Bits(bitsWidth bit))
  }

  val inMagic = RegInit(False)

  io.output.valid := io.input.valid
  io.output.payload := io.input.bits
  io.input.ready := io.output.ready

  when(inMagic) {
    when(io.input.isStart) {
      io.output.payload := cStart
    }
    when(io.input.isEnd) {
      io.output.payload := cEnd
    }
    when(io.output.fire) {
      inMagic := False
    }
  } otherwise {
    when(!io.input.isBits || io.input.bits === cMagic) {
      io.input.ready := False
      io.output.payload := cMagic
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
  io.output.bits := io.input.payload
  io.output.isStart := False
  io.output.isEnd := False

  when(io.input.fire) {
    when(inMagic) {
      switch(io.input.payload) {
        is(cStart) {
          io.output.valid := True
          io.output.isStart := True
        }
        is(cEnd) {
          io.output.valid := True
          io.output.isEnd := True
        }
        is(cMagic) {
          io.output.valid := True
        }
      }
      inMagic := False
    } otherwise {
      when(io.input.payload === cMagic) {
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

    val lastWriteData = RegNextWhen(io.input.bits(bitsWidth - 1 downto 0), pushFlag)

    when(pushFlag || flushFlag) {
      ram((writePtr - U(flushFlag)).resized) := (Mux(pushFlag, io.input.bits.resized, True ## lastWriteData)).resized
    }

    when(pushFlag) {
      checksum := checksum + U(io.input.bits) //TODO better checksum
      writePtr.increment()
    }
    when(flushFlag) {
      validPtr := writePtr.resized
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
      writePtr.valueNext := validPtr.resized
      checksum := 0
    }


    val readPtr = Counter(wordCountMax << 1)
    val readCmd = Stream(UInt(log2Up(wordCountMax) bit))
    readCmd.valid := (validPtr =/= readPtr)
    readCmd.payload := readPtr.resized
    readPtr.willIncrement := readCmd.fire
    io.output.translateFrom(ram.streamReadSync(readCmd))((to, from) => {
      to.last := from.msb
      to.fragment := from.resized
    })

  }


  val stateMachine = new Area {
    val state = RegInit(eIdle)
    val overflow = Reg(Bool)
    when(io.input.fire) {
      when(io.input.isBits) {
        switch(state) {
          is(eData) {
            overflow := overflow | !buffer.push
          }
          is(eCheck0) {
            when(io.input.bits === B(buffer.checksum(7 downto 0))) {
              state := eCheck1
            } otherwise {
              state := eIdle
            }
          }
          is(eCheck1) {
            when(!overflow && io.input.bits === B(buffer.checksum(15 downto 8))) {
              buffer.flush
            }
            state := eIdle
          }
        }
      }
      when(io.input.isStart) {
        state := eData
        overflow := False
        buffer.writeStart
      }
      when(io.input.isEnd) {
        state := eCheck0
      }
    }


  }
}
