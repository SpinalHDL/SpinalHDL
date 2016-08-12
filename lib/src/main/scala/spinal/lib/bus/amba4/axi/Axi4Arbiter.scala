package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

object Axi4ReadArbiter{
  def getInputConfig(outputConfig : Axi4Config,inputsCount : Int) = outputConfig.copy(idWidth = outputConfig.idWidth - log2Up(inputsCount))
}

case class Axi4ReadArbiter(outputConfig: Axi4Config,inputsCount : Int) extends Component {
  assert(outputConfig.isReadOnly)
  val inputConfig = Axi4ReadArbiter.getInputConfig(outputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(Axi4(inputConfig)),inputsCount)
    val output = master(Axi4(outputConfig))
  }

  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Ar(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.readCmd)).zipped.map(_ <> _)
  cmdArbiter.io.output <> io.output.readCmd

  io.output.readCmd.id.removeAssignements()
  io.output.readCmd.id := (cmdArbiter.io.chosen @@ cmdArbiter.io.output.id)

  // Route readResp
  val idPathRange = outputConfig.idWidth-1 downto outputConfig.idWidth - log2Up(inputsCount)
  val readRspSels = (0 until inputsCount).map(io.output.readRsp.id(idPathRange) === _)
  for((input,sel)<- (io.inputs,readRspSels).zipped){
    input.readRsp.valid := io.output.readRsp.valid && sel
    input.readRsp.payload <> io.output.readRsp.payload
    input.readRsp.id.removeAssignements()
    input.readRsp.id := io.output.readRsp.id(idPathRange.low-1 downto 0)
  }
  io.output.readRsp.ready := (readRspSels,io.inputs.map(_.readRsp.ready)).zipped.map(_ & _).reduce(_ | _)
}






object Axi4WriteArbiter{
  def getInputConfig(outputConfig : Axi4Config,inputsCount : Int) = outputConfig.copy(idWidth = outputConfig.idWidth - log2Up(inputsCount))
}

//routeBufferSize Specify how many write cmd could be schedule before any write data transaction is transmitted
case class Axi4WriteArbiter(outputConfig: Axi4Config,inputsCount : Int,routeBufferSize : Int) extends Component {
  assert(outputConfig.isWriteOnly)
  val inputConfig = Axi4ReadArbiter.getInputConfig(outputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(Axi4(inputConfig)),inputsCount)
    val output = master(Axi4(outputConfig))
  }

  // Route writeCmd
  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Aw(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.writeCmd)).zipped.map(_ <> _)
  val (cmdOutputFork,cmdRouteFork) = StreamFork2(cmdArbiter.io.output)
  io.output.writeCmd << cmdOutputFork
  io.output.writeCmd.id.removeAssignements()
  io.output.writeCmd.id := (cmdArbiter.io.chosen @@ cmdArbiter.io.output.id)
  
  // Route writeData
  val routeBuffer = cmdRouteFork.translateWith(cmdArbiter.io.chosen).queue(routeBufferSize) //TODO check queue minimal latency of queue (probably 2, which is bad)
  val routeDataInput = io.inputs(routeBuffer.payload).writeData
  io.output.writeData.valid := routeBuffer.valid && routeDataInput.valid
  io.output.writeData.payload  := routeDataInput.payload
  io.inputs.zipWithIndex.foreach{case(input,idx) => {
    input.writeData.ready := routeBuffer.valid && io.output.writeData.ready && routeBuffer.payload === idx
  }}
  routeBuffer.ready := io.output.writeData.fire && io.output.writeData.last

  // Route writeResp
  val idPathRange = outputConfig.idWidth-1 downto outputConfig.idWidth - log2Up(inputsCount)
  val writeRspSels = (0 until inputsCount).map(io.output.writeRsp.id(idPathRange) === _)
  for((input,sel)<- (io.inputs,writeRspSels).zipped){
    input.writeRsp.valid := io.output.writeRsp.valid && sel
    input.writeRsp.payload <> io.output.writeRsp.payload
    input.writeRsp.id.removeAssignements()
    input.writeRsp.id := io.output.writeRsp.id(idPathRange.low-1 downto 0)
  }
  io.output.writeRsp.ready := (writeRspSels,io.inputs.map(_.writeRsp.ready)).zipped.map(_ & _).reduce(_ | _)
}


