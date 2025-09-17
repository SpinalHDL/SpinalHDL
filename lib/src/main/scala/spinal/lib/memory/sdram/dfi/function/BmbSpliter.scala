package spinal.lib.memory.sdram.dfi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

//Because the BmbAlignedSpliter module in lib can't shard read data as well, some changes were made here.
object BmbAligner {
  def outputParameter(ip: BmbAccessParameter, alignmentWidth: Int) = {
    val beatCount = (1 << alignmentWidth) / ip.byteCount
    val transferCount = ip.transferBeatCount
    val readContext = if (ip.canRead) log2Up(beatCount) + log2Up(transferCount) else 0
    val aggregated = ip.aggregated
    var op =
      if (!bypass(ip, alignmentWidth))
        ip.withSingleSource(
          aggregated.copy(
            lengthWidth = aggregated.lengthWidth + 1,
            contextWidth = aggregated.contextWidth + 1 + readContext + ip.sourceWidth
          )
        )
      else {
        ip
      }

    op = op.sourcesTransform(
      _.copy(
        alignmentMin = alignmentWidth
      )
    )

    op
  }

  def bypass(ip: BmbAccessParameter, alignmentWidth: Int) =
    1 << alignmentWidth <= ip.byteCount && !ip.alignment.allowByte
}

//Extend access to have them aligned on a length of 2^alignmentWidth
case class BmbAligner(ip: BmbParameter, alignmentWidth: Int) extends Component {
  val op = BmbAligner.outputParameter(ip.access, alignmentWidth)

  val io = new Bundle {
    val input = slave(Bmb(ip))
    val output = master(Bmb(op))
  }
  val bypass = BmbAligner.bypass(ip.access, alignmentWidth)

  if (bypass) {
    io.output << io.input
    io.output.cmd.address(alignmentWidth - 1 downto 0) := 0
    io.output.cmd.length.allowOverride
    io.output.cmd.length(alignmentWidth - 1 downto 0) := (1 << alignmentWidth) - 1
  }

  val logic = if (!bypass) new Area {
    val beatCount = (1 << alignmentWidth) / ip.access.byteCount
    val transferCount = ip.access.transferBeatCount

    case class Context() extends Bundle {
      val write = Bool()
      val paddings = ip.access.canRead generate UInt(log2Up(beatCount) bits)
      val transfers = ip.access.canRead generate UInt(log2Up(transferCount) bits)
      val source = UInt(ip.access.sourceWidth bits)
      val input = Bits(ip.access.contextWidth bits)
    }

    val cmdLogic = new Area {
      io.output.cmd.valid := io.input.cmd.valid
      io.output.cmd.address := io.input.cmd.address(ip.access.addressWidth - 1 downto alignmentWidth) << alignmentWidth
      io.output.cmd.opcode := io.input.cmd.opcode
      io.output.cmd.length := (io.input.cmd.address(alignmentWidth - 1 downto 0) + io.input.cmd.length
        .resize(op.lengthWidth)) | ((1 << alignmentWidth) - 1)
      io.output.cmd.last := False

      val paddings = io.input.cmd.address(alignmentWidth - 1 downto log2Up(ip.access.byteCount))
      val context = Context()
      context.input := io.input.cmd.context
      context.write := io.input.cmd.isWrite
      context.source := io.input.cmd.source

      io.output.cmd.context := B(context)

      val inputReadyOk = False
      io.input.cmd.ready := io.output.cmd.ready && inputReadyOk

      val forWrite = ip.access.canWrite generate new Area {
        val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
        beatCounter := (beatCounter + U(io.output.cmd.fire && io.input.cmd.isWrite)).resized

        val prePadding = io.input.cmd.isWrite && io.input.cmd.first && beatCounter < paddings
        val postPadding =
          RegInit(False) setWhen (!prePadding && io.output.cmd.fire && io.input.cmd.last) clearWhen (io.input.cmd.ready)

        io.output.cmd.last setWhen (io.input.cmd.last && beatCounter === beatCount - 1)
        io.output.cmd.data := io.input.cmd.data
        io.output.cmd.mask := (!(prePadding || postPadding) ? io.input.cmd.mask | 0)
        inputReadyOk setWhen (!prePadding && !(io.input.cmd.last && beatCounter =/= beatCount - 1))
      }
      val lengthReg = RegNextWhen(io.input.cmd.length, io.input.cmd.fire, U((1 << ip.access.lengthWidth) - 1))
      val addressReg = RegNextWhen(io.input.cmd.address, io.input.cmd.fire, U(0))
      val length = cloneOf(io.input.cmd.length)
      val address = cloneOf(io.input.cmd.address)
      length := io.input.cmd.fire ? io.input.cmd.length | lengthReg
      address := io.input.cmd.fire ? io.input.cmd.address | addressReg
      def transferBeatCountMinusOne: UInt = {
        if (!ip.access.alignment.allowByte) {
          if (ip.access.lengthWidth < log2Up(ip.access.byteCount)) {
            U""
          } else {
            length(length.high downto log2Up(ip.access.byteCount))
          }
        } else {
          ((U"0" @@ length) + address(ip.access.wordRange))(length.high + 1 downto log2Up(ip.access.byteCount))
        }
      }
      val forRead = ip.access.canRead generate new Area {
        io.output.cmd.last setWhen (io.input.cmd.isRead)
        inputReadyOk setWhen (io.input.cmd.isRead)
        context.paddings := paddings
        context.transfers := transferBeatCountMinusOne
      }
    }

    val rspLogic = new Area {
      val context = io.output.rsp.context.as(Context())

      val drop = False
      io.input.rsp.arbitrationFrom(io.output.rsp.throwWhen(drop))
      io.input.rsp.last := False
      io.input.rsp.opcode := io.output.rsp.opcode
      io.input.rsp.context := context.input
      io.input.rsp.source := context.source

      val forWrite = ip.access.canWrite generate new Area {
        io.input.rsp.last.clearWhen(context.write).setWhen(io.output.rsp.lastFire)
      }

      val forRead = ip.access.canRead generate new Area {
        val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
        when(io.output.rsp.fire) {
          beatCounter := (beatCounter + U(!context.write)).resized
        }

        val transferCounter = Reg(UInt(log2Up(transferCount + 1) bits)) init (0)
        when(io.input.rsp.fire) {
          transferCounter := transferCounter + 1
        }
        when(io.output.rsp.fire && io.output.rsp.last) {
          transferCounter := 0
        }

        drop setWhen (!context.write && (io.input.rsp.first && beatCounter(
          context.paddings.range
        ) < context.paddings || transferCounter > context.transfers))

        io.input.rsp.last setWhen (transferCounter === context.transfers & (transferCounter =/= 0))
        io.input.rsp.data := io.output.rsp.data
      }

    }
  }

  // For simulation
  clockDomain.readClockWire
  clockDomain.readResetWire
}

object BmbAlignedSpliter {
  def outputParameter(ip: BmbAccessParameter, lengthMax: Int) = ip.withSingleSource(
    ip.aggregated.copy(
      lengthWidth = log2Up(lengthMax),
      contextWidth = ip.contextWidth + 2 + ip.sourceWidth
    )
  )
}

//Break big burst into multiple ones, not bigger than lengthMax and not crossing lengthMax address boundardy
case class BmbAlignedSpliter(ip: BmbParameter, lengthMax: Int) extends Component {
  val op = BmbAlignedSpliter.outputParameter(ip.access, lengthMax)

  val io = new Bundle {
    val input = slave(Bmb(ip))
    val output = master(Bmb(op))
    val outputBurstLast = out Bool ()
  }

  val beatCountMax = lengthMax / ip.access.byteCount
  val splitCountMax = ip.access.transferBeatCount / beatCountMax + (if (ip.access.alignment.allowWord) 1 else 0)
  val splitRange = log2Up(lengthMax) - 1 downto 0
  val addressRange = ip.access.addressWidth - 1 downto splitRange.high + 1
  val cmdLogic = new Area {
    val wrBeatCounter = Reg(UInt(log2Up(beatCountMax) bits)) init (0)
    val rdBeatCounter = Reg(UInt(log2Up(beatCountMax) bits)) init (0)
    val splitCounter = Reg(UInt(log2Up(splitCountMax) bits)) init (0)

    val lengthReg = RegNextWhen(io.input.cmd.length, io.input.cmd.fire, U(lengthMax - 1))
    val addressReg = RegNextWhen(io.input.cmd.address, io.input.cmd.fire, U(0))
    val length = cloneOf(io.input.cmd.length)
    val address = cloneOf(io.input.cmd.address)
    length := io.input.cmd.fire ? io.input.cmd.length | lengthReg
    address := io.input.cmd.fire ? io.input.cmd.address | addressReg

    val headLenghtMax = lengthMax - 1 - address(splitRange)
    val bodyLength = lengthMax - 1
    val lastAddress = address(splitRange) + (U"0" @@ length)
    val tailLength = lastAddress(splitRange)
    val splitCount = (lastAddress >> splitRange.size)

    val firstSplit = RegInit(True) clearWhen (io.output.cmd.lastFire)
    val lastSplit = splitCounter === splitCount
    val usedSplit = (splitCounter <= splitCount) && (splitCounter =/= 0)

    val addressBase = CombInit(address)
    when(!firstSplit) { addressBase(splitRange) := 0 }

    val beatsInSplit = U(lengthMax / ip.access.byteCount) - (~firstSplit ? U(0) | address(
      splitRange.high downto log2Up(ip.access.byteCount)
    ))

    val context = Context()
    context.input := io.input.cmd.fire ? io.input.cmd.context | RegNextWhen(io.input.cmd.context, io.input.cmd.fire)
    context.last := lastSplit
    context.write := io.output.cmd.isWrite
    context.source := io.input.cmd.source

    io.output.cmd.valid := io.input.cmd.valid | ((rdBeatCounter === beatsInSplit - 1) & usedSplit)
    io.output.cmd.last := io.input.cmd.last || ((wrBeatCounter === beatsInSplit - 1) & io.input.cmd.fire & io.input.cmd.isWrite) || ((rdBeatCounter === beatsInSplit - 1) & usedSplit)
    io.output.cmd.address := Bmb.addToAddress(addressBase, splitCounter << addressRange.low, ip)
    io.output.cmd.context := B(context)
    io.output.cmd.source := 0
    io.output.cmd.opcode := io.input.cmd.opcode & (~((rdBeatCounter === beatsInSplit - 1) & usedSplit).asBits)
    io.output.cmd.length := (firstSplit ## lastSplit) mux (
      B"10" -> headLenghtMax,
      B"00" -> U(lengthMax - 1),
      B"01" -> tailLength,
      B"11" -> length.resize(op.lengthWidth)
    )
    if (ip.access.canWrite) {
      io.output.cmd.data := io.input.cmd.data
      io.output.cmd.mask := io.input.cmd.mask
    }
    io.outputBurstLast := context.last
    io.input.cmd.ready := io.output.cmd.ready

    when(io.output.cmd.fire) {
      when(io.input.cmd.isWrite) {
        wrBeatCounter := wrBeatCounter + U(io.input.cmd.isWrite).resized
      }
      when(io.output.cmd.last) {
        splitCounter := splitCounter + U(1).resized
        wrBeatCounter := 0
      }
    }
    val rdStart =
      Reg(Bool()).init(False).setWhen(io.input.cmd.valid & io.input.cmd.isRead).clearWhen(io.input.rsp.lastFire)
    when(rdStart) {
      rdBeatCounter := rdBeatCounter + 1
      when(io.output.cmd.lastFire) {
        rdBeatCounter := 0
      }
    } otherwise (rdBeatCounter.clearAll())

    when((io.input.cmd.lastFire & io.input.cmd.isWrite)) {
      firstSplit := True
    }
    when(io.output.cmd.lastFire & lastSplit) {
      splitCounter := 0
    }
  }
  val rspLogic = new Area {
    val context = io.output.rsp.context.as(Context())
    io.input.rsp.valid := io.output.rsp.valid & (context.write ? context.last | True)
    io.output.rsp.ready := io.input.rsp.ready
    io.input.rsp.last := io.output.rsp.last && context.last
    io.input.rsp.source := context.source
    io.input.rsp.opcode := io.output.rsp.opcode
    if (ip.access.canRead) {
      io.input.rsp.data := io.output.rsp.data
    }
    io.input.rsp.context := context.input
  }

  case class Context() extends Bundle {
    val source = UInt(ip.access.sourceWidth bits)
    val last = Bool()
    val write = Bool()
    val input = Bits(ip.access.contextWidth bits)
  }
}
