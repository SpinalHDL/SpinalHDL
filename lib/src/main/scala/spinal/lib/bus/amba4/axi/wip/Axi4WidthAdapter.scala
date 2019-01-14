package spinal.lib.bus.amba4.axi.wip

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}

class Dummy
//object Axi4SharedWidthDivider{
//  def getOutputConfig(inputConfig : Axi4Config,factor : Int) = inputConfig.copy(dataWidth = inputConfig.dataWidth/factor)
//}
//
////Work in progress, do not use
//case class Axi4SharedWidthDivider(inputConfig : Axi4Config,factor : Int) extends Component{
//  require(isPow2(factor))
//  require(factor >= 2)
//  val outputConfig = Axi4SharedWidthDivider.getOutputConfig(inputConfig,factor)
//  require(outputConfig.dataWidth >= 8)
//
//  val io = new Bundle{
//    val input  = slave(Axi4Shared(inputConfig))
//    val output = master(Axi4Shared(inputConfig))
//  }
//
//
//  val (cmdFork,dataForkTmp) = StreamFork2(io.input.sharedCmd)
//
//
//  val sharedCmd = new Area {
//    io.output.sharedCmd << cmdFork
//    for(i <- log2Up(outputConfig.bytePerWord) + 1 to log2Up(inputConfig.bytePerWord)){
//      when(i === cmdFork.len) {
//        io.output.sharedCmd.size := log2Up(outputConfig.bytePerWord)
//        io.output.sharedCmd.len := ((cmdFork.len << i) @@ U((i - 1 downto 0) -> true)).resized
//      }
//    }
//  }
//
//  val writeData = new Area {
//    val dataFork = dataForkTmp.throwWhen(!dataForkTmp.write)
//    val writeData = io.input.writeData.haltWhen(!dataFork.valid)
//    val symbolSelect = UInt(log2Up(inputConfig.bytePerWord) bits)
//    val symbolCounter = Reg(symbolSelect)
//    val writeFirstBeat = RegInit(True)
//    val outputBeatSize = Min[UInt](dataFork.size, log2Up(outputConfig.bytePerWord))
//
//    when(dataFork.size <= log2Up(outputConfig.bytePerWord)) {
//      writeData.ready := io.output.writeData.ready
//    } otherwise {
//      writeData.ready := io.output.writeData.ready && sy
//    }
//
//    when(writeFirstBeat) {
//      symbolSelect := dataFork.addr(symbolSelect.range)
//    } otherwise {
//      symbolSelect := symbolCounter
//    }
//    when(io.output.writeData.ready) {
//      symbolCounter := symbolSelect + outputBeatSize + 1
//    }
//
//    io.output.writeData.arbitrationFrom(writeData.haltWhen(!dataFork.valid))
//    io.output.writeData.data := writeData.data.vecSplit(factor).read(symbolSelect)
//    if (inputConfig.useStrb) io.output.writeData.strb := writeData.strb.vecSplit(factor).read(symbolSelect)
//    if (inputConfig.useUser) io.output.writeData.user := writeData.user
//    if (inputConfig.useLast) io.output.writeData.last := writeData.last && XX
//    dataFork.ready := io.output.writeData.fire && io.output.writeData.last
//  }
//}
