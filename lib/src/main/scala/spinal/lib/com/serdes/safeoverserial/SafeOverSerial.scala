package spinal.lib.com.serdes.safeoverserial

import spinal.core._
import spinal.lib._

object SafeSerialConst {
  def cMagic = b"xA5"
  def cStart = b"xD8"
  def cEnd = b"x9A"


  def bitsWidth = 8
}


//object SafeSerialConst {
//  def cStart0 = b"xA5"
//  def cStart1 = b"xD8"
//  def cData = b"x02"
//  def cAck = b"x03"
//  def cError = b"x04"
//  def cEnd = b"x05"
//
//  def bitsWidth = 8
//}

class SafeSerialTx(bitsWidth: Int) extends Component {
  val io = new Bundle {
    val input = slave Handshake (Bits(bitsWidth bit))
    val output = master Handshake (Bits(bitsWidth bit))
  }
}


object SafeSerialRxState extends SpinalEnum {
  val idle, data, check0, check1 = Value
}


class SafeSerialRx(wordCountMax: Int) extends Component {

  import SafeSerialRxState._
  import SafeSerialConst._

  val io = new Bundle {
    val input = slave Flow (Bits(bitsWidth bit))
    val output = master Handshake (Bits(bitsWidth bit))
  }

  val buffer = new Area {
    val ram = Mem(Bits(bitsWidth bit), wordCountMax)
    val writePtr = Counter(wordCountMax)
    val validPtr = Reg(UInt(log2Up(wordCountMax) bit)) init (0)

    val checksum = Reg(UInt(16 bit))


    def write(that: Bits): Bool = {
      val success = !(writePtr === readPtr && risingOccupancy)
      when(success) {
        ram(writePtr) := that
        checksum := checksum + toUInt(that) //TODO better checksum
        writePtr ++
      }
      success
    }


    def flush: Unit = {
      validPtr := writePtr
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
    io.output << ram.handshakeReadSync(readCmd)

    val risingOccupancy = RegInit(False)
    when(writePtr.inc !== readPtr.inc) {
      risingOccupancy := writePtr.inc
    }
  }

  val stateMachine = new Area {
    val state = RegInit(idle)
    val inMagic = RegInit(False)
    val consumeData = False
    val overflow = Reg(Bool)
    when(io.input.fire) {
      when(inMagic) {
        switch(io.input.data) {
          is(cStart) {
            state := data
            overflow := False
            buffer.writeStart
          }
          is(cEnd) {
            state := check0
          }
          is(cMagic) {
            consumeData := True
          }
          default {
            state := idle
          }
        }
      } otherwise {
        when(io.input.data === cMagic) {
          inMagic := True
        } otherwise {
          consumeData := True
        }
      }
    }

    when(consumeData) {
      switch(state) {
        is(data) {
          overflow := overflow | !buffer.write(io.input.data)
        }
        is(check0) {
          when(io.input.data === buffer.checksum(7, 0)) {
            state := check1
          } otherwise {
            state := idle
          }
        }
        is(check1) {
          when(!overflow && io.input.data === buffer.checksum(15, 8)) {
            buffer.flush
          }
          state := idle
        }
      }
    }
  }
}
