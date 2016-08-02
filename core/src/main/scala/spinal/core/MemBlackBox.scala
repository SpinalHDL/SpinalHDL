package spinal.core

class Ram_1c_1w_1ra(wordWidth: Int, wordCount: Int, maskWidth : Int = 1, useMask : Boolean = false,readUnderWrite: ReadUnderWritePolicy = dontCare,tech : MemTechnologyKind = auto) extends BlackBox {
  if (readUnderWrite == readFirst) SpinalError("readFirst mode for asyncronous read is not alowed")

  val generic = new Generic {
    val wordCount = Ram_1c_1w_1ra.this.wordCount
    val wordWidth = Ram_1c_1w_1ra.this.wordWidth
    val maskWidth = Ram_1c_1w_1ra.this.maskWidth
    val readUnderWrite = Ram_1c_1w_1ra.this.readUnderWrite.readUnderWriteString
    val tech = Ram_1c_1w_1ra.this.tech.technologyKind
    val useMask = Ram_1c_1w_1ra.this.useMask
  }

  val io = new Bundle {
    val clk = in Bool

    val wr = new Bundle {
      val en = in Bool
      val mask = in Bits(maskWidth bits)
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data,if(useMask) io.wr.mask else null)
  }
  io.rd.data := mem.readAsync(io.rd.addr,readUnderWrite)
}

class Ram_1w_1rs(wordWidth: Int,
                    wordCount: Int,
                    wrClock : ClockDomain,
                    rdClock : ClockDomain,
                    writeAddressWidth : Int,
                    writeDataWidth : Int,
                    readAddressWidth : Int,
                    readDataWidth : Int,
                    writeMaskWidth : Int = 1,
                    useMask : Boolean = false,
                    readUnderWrite: ReadUnderWritePolicy = dontCare,
                    tech : MemTechnologyKind = auto) extends BlackBox {
  
  val generic = new Generic {
    val wordCount = Ram_1w_1rs.this.wordCount
    val wordWidth = Ram_1w_1rs.this.wordWidth
    val writeAddressWidth = Ram_1w_1rs.this.writeAddressWidth
    val writeDataWidth = Ram_1w_1rs.this.writeDataWidth
    val readAddressWidth = Ram_1w_1rs.this.readAddressWidth
    val readDataWidth = Ram_1w_1rs.this.readDataWidth
    val maskWidth = Ram_1w_1rs.this.writeMaskWidth
    val readUnderWrite = Ram_1w_1rs.this.readUnderWrite.readUnderWriteString
    val tech = Ram_1w_1rs.this.tech.technologyKind
    var useReadEnable = true
    var clockCrossing = wrClock != rdClock
    val useMask = Ram_1w_1rs.this.useMask
  }

  val io = new Bundle {

    val wr = new Bundle {
      val clk = in Bool
      val en = in Bool
      val mask = in Bits (writeMaskWidth bits)
      val addr = in UInt (writeAddressWidth bit)
      val data = in Bits (writeDataWidth bit)
    }
    val rd = new Bundle {
      val clk = in Bool
      val en = in Bool
      val addr = in  UInt (readAddressWidth bit)
      val data = out Bits (readDataWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
//  val mem = Mem(io.wr.data, wordCount)
//  when(io.wr.en) {
//    mem.write(io.wr.addr, io.wr.data,if(useMask) io.wr.mask else null)
//  }
//  io.rd.data := mem.readSync(io.rd.addr, io.rd.en,writeToReadKind)
}


class Ram_2c_1w_1rs(wordWidth: Int,
                    wordCount: Int,
                    wrClock : ClockDomain,
                    rdClock : ClockDomain,
                    maskWidth : Int = 1,
                    useMask : Boolean = false,
                    readUnderWrite: ReadUnderWritePolicy = dontCare,
                    tech : MemTechnologyKind = auto) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_2c_1w_1rs.this.wordCount
    val wordWidth = Ram_2c_1w_1rs.this.wordWidth
    val maskWidth = Ram_2c_1w_1rs.this.maskWidth
    val readUnderWrite = Ram_2c_1w_1rs.this.readUnderWrite.readUnderWriteString
    val tech = Ram_2c_1w_1rs.this.tech.technologyKind
    var useReadEnable = true
    val useMask = Ram_2c_1w_1rs.this.useMask
  }

  val io = new Bundle {
    val wr = new Bundle {
      val clk = in Bool
      val en = in Bool
      val mask = in Bits(maskWidth bits)
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val clk = in Bool
      val en = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  mapClockDomain(wrClock,io.wr.clk)
  mapClockDomain(rdClock,io.rd.clk)
  noIoPrefix()

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  new ClockingArea(wrClock) {
    when(io.wr.en) {
      mem.write(io.wr.addr, io.wr.data, if (useMask) io.wr.mask else null)
    }
  }
  new ClockingArea(rdClock) {
    io.rd.data := mem.readSync(io.rd.addr, io.rd.en,readUnderWrite,clockCrossing = true)
  }
}

class Ram_1wrs(wordWidth: Int, wordCount: Int, readUnderWrite: ReadUnderWritePolicy = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1wrs.this.wordCount
    val wordWidth = Ram_1wrs.this.wordWidth
    val readUnderWrite = Ram_1wrs.this.readUnderWrite.readUnderWriteString
    var useReadEnable = true
  }

  val io = new Bundle {
    val clk = in Bool

    val addr = in UInt (log2Up(wordCount) bit)

    val wr = new Bundle {
      val en = in Bool
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en = in Bool
      val data = out Bits (wordWidth bit)
    }
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.addr, io.wr.data)
  }
  io.rd.data := mem.readSync(io.addr, io.rd.en)
}


class Ram_1wors(wordWidth: Int, wordCount: Int, readUnderWrite: ReadUnderWritePolicy = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1wors.this.wordCount
    val wordWidth = Ram_1wors.this.wordWidth
    val readUnderWrite = Ram_1wors.this.readUnderWrite.readUnderWriteString
  }

  val io = new Bundle {
    val clk = in Bool

    val cs = in Bool
    val we = in Bool
    val addr = in UInt (log2Up(wordCount) bit)
    val wrData = in Bits (wordWidth bit)
    val rdData = out Bits (wordWidth bit)
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wrData, wordCount)
  io.rdData := mem.readWriteSync(io.addr, io.wrData, io.cs, io.we, readUnderWrite)
}