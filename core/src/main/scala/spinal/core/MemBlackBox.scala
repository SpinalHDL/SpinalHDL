package spinal.core

class Ram_1c_1w_1ra(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  if (writeToReadKind == readFirst) SpinalError("readFirst mode for asyncronous read is not alowed")

  val generic = new Generic {
    val wordCount = Ram_1c_1w_1ra.this.wordCount
    val wordWidth = Ram_1c_1w_1ra.this.wordWidth
    val readToWriteKind = writeToReadKind.toString
  }

  val io = new Bundle {
    val clk = in Bool()

    val wr = new Bundle {
      val en = in Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  useCurrentClockDomain(io.clk)

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data)
  }
  io.rd.data := mem.readAsync(io.rd.addr)
}

class Ram_1c_1w_1rs(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1c_1w_1rs.this.wordCount
    val wordWidth = Ram_1c_1w_1rs.this.wordWidth
    val readToWriteKind = writeToReadKind.toString
    var useReadEnable = true
  }

  val io = new Bundle {
    val clk = in Bool()

    val wr = new Bundle {
      val en = in Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en = in Bool()
      val addr = in UInt (log2Up(wordCount) bit)
      val data = out Bits (wordWidth bit)
    }
  }

  useCurrentClockDomain(io.clk)

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.wr.addr, io.wr.data)
  }
  io.rd.data := mem.readSync(io.rd.addr, io.rd.en)
}



class Ram_1wrs(wordWidth: Int, wordCount: Int, writeToReadKind: MemWriteToReadKind = dontCare) extends BlackBox {
  val generic = new Generic {
    val wordCount = Ram_1wrs.this.wordCount
    val wordWidth = Ram_1wrs.this.wordWidth
    val readToWriteKind = writeToReadKind.toString
    var useReadEnable = true
  }

  val io = new Bundle {
    val clk = in Bool()

    val addr = in UInt (log2Up(wordCount) bit)

    val wr = new Bundle {
      val en = in Bool()
      val data = in Bits (wordWidth bit)
    }
    val rd = new Bundle {
      val en = in Bool()
      val data = out Bits (wordWidth bit)
    }
  }

  useCurrentClockDomain(io.clk)

  def useReadEnable = io.rd.en.getLiteral[BoolLiteral]

  //Following is not obligatory, just to describe blackbox logic
  val mem = Mem(io.wr.data, wordCount)
  when(io.wr.en) {
    mem.write(io.addr, io.wr.data)
  }
  io.rd.data := mem.readSync(io.addr, io.rd.en)
}
