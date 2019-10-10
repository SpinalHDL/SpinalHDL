package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._

object BmbAligner{
  def bypass(ip : BmbParameter, alignmentWidth : Int) = 1 << alignmentWidth <= ip.byteCount && !ip.alignment.allowByte
  def outputParameter(ip : BmbParameter, alignmentWidth : Int) = {
    val beatCount = (1 << alignmentWidth) / ip.byteCount
    val transferCount = ip.transferBeatCount

    var op = if(!bypass(ip, alignmentWidth)) ip.copy(
      lengthWidth = ip.lengthWidth + 1,
      contextWidth = ip.contextWidth + 1 + log2Up(beatCount) + log2Up(transferCount) + ip.sourceWidth,
      sourceWidth = 0
    ) else {
      ip
    }

    op = op.copy(
      alignmentMin = alignmentWidth
    )

    op
  }
}

//Extend access to have them aligned on a length of 2^alignmentWidth
case class BmbAligner(ip : BmbParameter, alignmentWidth : Int) extends Component{
  val op = BmbAligner.outputParameter(ip, alignmentWidth)

  val io = new Bundle{
    val input = slave(Bmb(ip))
    val output = master(Bmb(op))
  }
  val bypass = BmbAligner.bypass(ip, alignmentWidth)

  if(bypass){
    io.output << io.input
    io.output.cmd.address(alignmentWidth-1 downto 0) := 0
    io.output.cmd.length.allowOverride
    io.output.cmd.length(alignmentWidth-1 downto 0) := (1 << alignmentWidth)-1
  }

  val logic = if(!bypass) new Area {
    val beatCount = (1 << alignmentWidth) / ip.byteCount
    val transferCount = ip.transferBeatCount

    case class Context() extends Bundle {
      val input = Bits(ip.contextWidth bits)
      val write = Bool()
      val paddings = UInt(log2Up(beatCount) bits)
      val transfers = UInt(log2Up(transferCount) bits)
      val source = UInt(ip.sourceWidth bits)
    }

    val cmdLogic = new Area {
      val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
      beatCounter := beatCounter + U(io.output.cmd.fire && io.input.cmd.isWrite)

      val context = Context()
      context.input := io.input.cmd.context
      context.paddings := io.input.cmd.address(alignmentWidth - 1 downto log2Up(ip.byteCount))
      context.transfers := io.input.cmd.transferBeatCountMinusOne
      context.write := io.input.cmd.isWrite
      context.source := io.input.cmd.source

      val prePadding = io.input.cmd.isWrite && io.input.cmd.first && beatCounter < context.paddings
      val postPadding = RegInit(False) setWhen(!prePadding && io.output.cmd.fire && io.input.cmd.last) clearWhen(io.input.cmd.ready)

      io.output.cmd.valid := io.input.cmd.valid
      io.output.cmd.last := io.input.cmd.isRead || io.input.cmd.last && beatCounter === beatCount-1
      io.output.cmd.address := io.input.cmd.address(ip.addressWidth - 1 downto alignmentWidth) << alignmentWidth
      io.output.cmd.context := B(context)
      io.output.cmd.opcode := io.input.cmd.opcode
      io.output.cmd.length := (io.input.cmd.address(alignmentWidth-1 downto 0) + io.input.cmd.length.resize(op.lengthWidth)) | ((1 << alignmentWidth) - 1)
      io.output.cmd.data := io.input.cmd.data
      io.output.cmd.mask := (!(prePadding || postPadding) ? io.input.cmd.mask | 0)
      io.input.cmd.ready := io.output.cmd.ready && (io.output.cmd.isRead || !prePadding && !(io.input.cmd.last && beatCounter =/= beatCount-1))
    }

    val rspLogic = new Area {
      val context = io.output.rsp.context.as(Context())

      val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
      when(io.output.rsp.fire) {
        beatCounter := beatCounter + U(!context.write)
      }

      val transferCounter = Reg(UInt(log2Up(transferCount + 1) bits)) init (0)
      when(io.input.rsp.fire) {
        transferCounter := transferCounter + 1
      }
      when(io.output.rsp.fire && io.output.rsp.last){
        transferCounter := 0
      }

      val drop = !context.write && (io.input.rsp.first && beatCounter(context.paddings.range) < context.paddings || transferCounter > context.transfers)

      io.input.rsp.arbitrationFrom(io.output.rsp.throwWhen(drop))
      io.input.rsp.last := context.write || transferCounter === context.transfers
      io.input.rsp.opcode := io.output.rsp.opcode
      io.input.rsp.data := io.output.rsp.data
      io.input.rsp.context := io.output.rsp.context.resized
      io.input.rsp.source := context.source
    }
  }
}

object BmbLengthFixer{
  def outputParameter(ip : BmbParameter, fixedWidth : Int) = ip.copy(
    lengthWidth = fixedWidth,
    alignmentMin = fixedWidth,
    contextWidth = ip.contextWidth + 2
  )
}

//Subdivide a burst into smaller fixed length bursts
//Require the input to be fixedLength aligned
//Warning, Can fire output cmd before input cmd on reads
case class BmbLengthFixer(ip : BmbParameter, fixedWidth : Int) extends Component{
  val op = BmbLengthFixer.outputParameter(ip, fixedWidth)

  val io = new Bundle{
    val input = slave(Bmb(ip))
    val output = master(Bmb(op))
    val outputBurstLast = out Bool()
  }

  val beatCount = (1 << fixedWidth) / ip.byteCount
  val splitCount = ip.transferBeatCount / beatCount

  case class Context() extends Bundle{
    val input = Bits(ip.contextWidth bits)
    val last = Bool()
    val write = Bool()
  }

  val cmdLogic = new Area {
    val fixedAddress = io.input.cmd.address(ip.addressWidth - 1 downto Bmb.boundaryWidth)
    val baseAddress = io.input.cmd.address(Bmb.boundaryWidth - 1 downto op.lengthWidth)
    val beatAddress = io.input.cmd.address(op.lengthWidth - 1 downto log2Up(op.byteCount))

    val beatCounter = Reg(UInt(log2Up(beatCount) bits)) init (0)
    val splitCounter = Reg(UInt(log2Up(splitCount) bits)) init (0)

    val context = Context()
    context.input := io.input.cmd.context
    context.last := splitCounter === (io.input.cmd.length >> fixedWidth)
    context.write := io.input.cmd.isWrite

    io.output.cmd.valid := io.input.cmd.valid
    io.output.cmd.last := io.input.cmd.last || beatCounter === beatCount-1
    io.output.cmd.address := (fixedAddress @@ (baseAddress + splitCounter)) << op.lengthWidth
    io.output.cmd.context := B(context)
    io.output.cmd.source := io.input.cmd.source
    io.output.cmd.opcode := io.input.cmd.opcode
    io.output.cmd.length := (1 << fixedWidth) - 1
    io.output.cmd.data := io.input.cmd.data
    io.output.cmd.mask := io.input.cmd.mask
    io.outputBurstLast := context.last
    io.input.cmd.ready := io.output.cmd.ready && (io.input.cmd.isWrite || context.last)

    when(io.output.cmd.fire){
      beatCounter := beatCounter + U(io.input.cmd.isWrite).resized
      when(io.output.cmd.last){
        splitCounter := splitCounter + U(1).resized
      }
    }
    when(io.input.cmd.fire && io.input.cmd.last){
      splitCounter := 0
    }
  }

  val rspLogic = new Area{
    val context = io.output.rsp.context.as(Context())
    io.input.rsp.arbitrationFrom(io.output.rsp.takeWhen(!context.write || context.last && io.output.rsp.last))
    io.input.rsp.last := io.output.rsp.last && context.last
    io.input.rsp.source := io.output.rsp.source
    io.input.rsp.opcode := io.output.rsp.opcode
    io.input.rsp.data := io.output.rsp.data
    io.input.rsp.context := io.output.rsp.context.resized
  }
}