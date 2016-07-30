package spinal.core

class Ram_1c_1w_1ra(wordWidth: Int, wordCount: Int, maskWidth : Int = 1, useMask : Boolean = false,writeToReadKind: MemWriteToReadKind = dontCare,tech : MemTechnologyKind = auto) extends BlackBox {
  if (writeToReadKind == readFirst) SpinalError("readFirst mode for asyncronous read is not alowed")

  val generic = new Generic {
    val wordCount = Ram_1c_1w_1ra.this.wordCount
    val wordWidth = Ram_1c_1w_1ra.this.wordWidth
    val maskWidth = Ram_1c_1w_1ra.this.maskWidth
    val readToWriteKind = writeToReadKind.writeToReadKind
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
  io.rd.data := mem.readAsync(io.rd.addr,writeToReadKind)
}

class Ram_1c_1w_1rs(wordWidth: Int, wordCount: Int, maskWidth : Int = 1, useMask : Boolean = false,writeToReadKind: MemWriteToReadKind = dontCare,tech : MemTechnologyKind = auto) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1c_1w_1rs.this.wordCount
    val wordWidth = Ram_1c_1w_1rs.this.wordWidth
    val maskWidth = Ram_1c_1w_1rs.this.maskWidth
    val readToWriteKind = writeToReadKind.writeToReadKind
    val tech = Ram_1c_1w_1rs.this.tech.technologyKind
    var useReadEnable = true
    val useMask = Ram_1c_1w_1rs.this.useMask
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
      val en = in Bool
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  mapCurrentClockDomain(io.clk)
  noIoPrefix()

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data,if(useMask) io.wr.mask else null)
  }
  io.rd.data := mem.readSync(io.rd.addr, io.rd.en,writeToReadKind)
}


class Ram_2c_1w_1rs(wordWidth: Int,
                    wordCount: Int,
                    wrClock : ClockDomain,
                    rdClock : ClockDomain,
                    maskWidth : Int = 1,
                    useMask : Boolean = false,
                    writeToReadKind: MemWriteToReadKind = dontCare,
                    tech : MemTechnologyKind = auto) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_2c_1w_1rs.this.wordCount
    val wordWidth = Ram_2c_1w_1rs.this.wordWidth
    val maskWidth = Ram_2c_1w_1rs.this.maskWidth
    val readToWriteKind = writeToReadKind.writeToReadKind
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
    io.rd.data := mem.readSyncCC(io.rd.addr, io.rd.en,writeToReadKind)
  }
}

class Ram_1wrs(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1wrs.this.wordCount
    val wordWidth = Ram_1wrs.this.wordWidth
    val readToWriteKind = writeToReadKind.writeToReadKind
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


class Ram_1wors(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1wors.this.wordCount
    val wordWidth = Ram_1wors.this.wordWidth
    val readToWriteKind = writeToReadKind.writeToReadKind
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
  io.rdData := mem.writeOrReadSync(io.addr, io.wrData, io.cs, io.we, writeToReadKind)
}