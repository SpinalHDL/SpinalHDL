package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

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
      byteCounter := io.input.writeCmd.addr.resized
      size := io.input.writeCmd.size
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
