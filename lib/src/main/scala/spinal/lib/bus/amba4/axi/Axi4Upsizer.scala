package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._


//Curently only INCR burst compatible
case class Axi4WriteOnlyUpsizer(inputConfig : Axi4Config, outputConfig : Axi4Config) extends Component {
  val io = new Bundle {
    val input = slave(Axi4WriteOnly(inputConfig))
    val output = master(Axi4WriteOnly(outputConfig))
  }

  val sizeMax = log2Up(outputConfig.bytePerWord)
  val cmdLogic = new Area{
    val (outputFork, dataFork) = StreamFork2(io.input.writeCmd)
    io.output.writeCmd << outputFork
//    val len = (io.input.writeCmd.len >> (sizeMax - io.input.writeCmd.size)) +
    val byteCount = (io.input.writeCmd.len << io.input.writeCmd.size).resize(12 bits)
    val incrLen = ((U"0" @@ byteCount) + io.input.writeCmd.addr(outputConfig.symbolRange))(byteCount.high + 1 downto log2Up(outputConfig.bytePerWord))
    when(io.output.writeCmd.isINCR()){
      io.output.writeCmd.size := sizeMax
      io.output.writeCmd.len := incrLen.resized
    }
  }

  val dataLogic = new Area{
    val byteCounter = Reg(UInt(log2Up(outputConfig.bytePerWord) bits))
    val size = Reg(UInt(3 bits))
    val outputValid = RegInit(False) clearWhen(io.output.writeData.ready)
    val outputLast = Reg(Bool)
    val busy = RegInit(False)
    val incrementByteCounter, alwaysFire = Reg(Bool)
    val byteCounterNext = (U"0" @@ byteCounter) + (U"1" << size).resized
    val dataBuffer = Reg(Bits(outputConfig.dataWidth bits))
    val maskBuffer = Reg(Bits(outputConfig.bytePerWord bits)) init(0)
    val byteActivity = (0 until sizeMax).map(sizeValue => U((1 << (1 << sizeValue))-1, outputConfig.bytePerWord bits)).read(size.resized) |<< byteCounter

    when(io.output.writeData.fire){
      maskBuffer := 0
    }

    io.output.writeData.valid := outputValid
    io.input.writeData.ready := busy && !io.output.writeData.isStall
    io.output.writeData.data := dataBuffer
    io.output.writeData.strb := maskBuffer
    io.output.writeData.last := outputLast

    when(io.input.writeData.fire){
      outputValid := byteCounterNext(widthOf(byteCounter)) || io.input.writeData.last || alwaysFire
      when(incrementByteCounter) {
        byteCounter := byteCounterNext.resized
      }
      busy clearWhen(io.input.writeData.last)
      outputLast := io.input.writeData.last
      for(outputByte <- 0 until outputConfig.bytePerWord) when(byteActivity(outputByte)){
        val inputByte = outputByte % inputConfig.bytePerWord
        dataBuffer(outputByte*8+7 downto outputByte*8) := io.input.writeData.data(inputByte*8+7 downto inputByte*8)
        maskBuffer(outputByte) := io.input.writeData.strb(inputByte)
      }
    }

    when(cmdLogic.dataFork.fire){
      byteCounter := cmdLogic.dataFork.addr.resized
      for(bit <- 0 until Math.min(7,widthOf(byteCounter))) byteCounter(bit) clearWhen(cmdLogic.dataFork.size > bit)
      size := cmdLogic.dataFork.size
      alwaysFire := !cmdLogic.dataFork.isINCR()
      incrementByteCounter := !cmdLogic.dataFork.isFIXED
      busy := True
    }

    cmdLogic.dataFork.ready := !busy
  }

  val rspLogic = new Area {
    io.input.writeRsp << io.output.writeRsp
  }
}

//Currently only INCR compatible
case class Axi4ReadOnlyUpsizer(inputConfig : Axi4Config, outputConfig : Axi4Config, pendingQueueSize : Int) extends Component {
  val io = new Bundle {
    val input = slave(Axi4ReadOnly(inputConfig))
    val output = master(Axi4ReadOnly(outputConfig))
  }

  val sizeMax = log2Up(outputConfig.bytePerWord)
  val ratio = outputConfig.dataWidth / inputConfig.dataWidth

  case class RspContext() extends Bundle{
    val startAt, endAt = Reg(UInt(log2Up(outputConfig.bytePerWord) bits))
    val size = UInt(3 bits)
    val id = UInt(inputConfig.idWidth bits)
  }

  val cmdLogic = new Area{
    val (outputFork, dataFork) = StreamFork2(io.input.readCmd)
    io.output.readCmd << outputFork
    val byteCount = (io.input.readCmd.len << io.input.readCmd.size).resize(12 bits)
    val incrLen = ((U"0" @@ byteCount) + io.input.readCmd.addr(outputConfig.symbolRange))(byteCount.high + 1 downto log2Up(outputConfig.bytePerWord))

    io.output.readCmd.size.removeAssignments() := sizeMax
    io.output.readCmd.len.removeAssignments() := incrLen.resized
    io.output.readCmd.id.removeAssignments() := 0 //Do not allow out of order
  }

  val dataLogic = new Area{
    val cmdPush = Stream(RspContext())
    cmdPush.arbitrationFrom(cmdLogic.dataFork)
    cmdPush.startAt := cmdLogic.dataFork.addr.resized
    cmdPush.endAt := (cmdLogic.dataFork.addr + (cmdLogic.dataFork.len << cmdLogic.dataFork.size)).resized
    cmdPush.size := cmdLogic.dataFork.size
    cmdPush.id := cmdLogic.dataFork.id

    val cmdPop = cmdPush.queue(pendingQueueSize)

    val size = Reg(UInt(3 bits))
    val busy = RegInit(False)
    val id = Reg(UInt(inputConfig.idWidth bits))
    val byteCounter = Reg(UInt(log2Up(outputConfig.bytePerWord) bits))
    val byteCounterLast = Reg(UInt(log2Up(outputConfig.bytePerWord) bits))
    val byteCounterNext = (U"0" @@ byteCounter) + (U"1" << size).resized


    when(cmdPop.fire){
      byteCounter := cmdPop.startAt
      byteCounterLast := cmdPop.endAt
      size := cmdPop.size
      id := cmdPop.id
      busy := True
    }

    cmdPop.ready := !busy


    when(io.input.readRsp.fire){
      byteCounter := byteCounterNext.resized
      busy clearWhen(io.input.readRsp.last)
    }

    io.input.readRsp.valid := io.output.readRsp.valid && busy
    io.input.readRsp.last := io.output.readRsp.last && byteCounter === byteCounterLast
    io.input.readRsp.resp := io.output.readRsp.resp
    io.input.readRsp.data := io.output.readRsp.data.subdivideIn(ratio.slices).read(byteCounter >> log2Up(inputConfig.bytePerWord))
    io.input.readRsp.id := id
    io.output.readRsp.ready := busy && io.input.readRsp.ready && (io.input.readRsp.last || byteCounterNext(widthOf(byteCounter)))
  }
}



case class Axi4Upsizer(inputConfig : Axi4Config,
                       outputConfig : Axi4Config,
                       readPendingQueueSize : Int) extends Component{
  val io = new Bundle {
    val input = slave(Axi4(inputConfig))
    val output = master(Axi4(outputConfig))
  }

  val readOnly = Axi4ReadOnlyUpsizer(inputConfig, outputConfig, readPendingQueueSize)
  val writeOnly = Axi4WriteOnlyUpsizer(inputConfig, outputConfig)

  readOnly.io.input.ar <> io.input.ar
  readOnly.io.input.r <> io.input.r
  writeOnly.io.input.aw <> io.input.aw
  writeOnly.io.input.w <> io.input.w
  writeOnly.io.input.b <> io.input.b

  readOnly.io.output.ar <> io.output.ar
  readOnly.io.output.r <> io.output.r
  writeOnly.io.output.aw <> io.output.aw
  writeOnly.io.output.w <> io.output.w
  writeOnly.io.output.b <> io.output.b
}