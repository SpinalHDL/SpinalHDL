package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

object Axi4ReadOnlyArbiter{
  def getInputConfig(outputConfig : Axi4Config,inputsCount : Int) = outputConfig.copy(idWidth = outputConfig.idWidth - log2Up(inputsCount))
}

case class Axi4ReadOnlyArbiter(outputConfig: Axi4Config,inputsCount : Int) extends Component {
  val inputConfig = Axi4ReadOnlyArbiter.getInputConfig(outputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(Axi4ReadOnly(inputConfig)),inputsCount)
    val output = master(Axi4ReadOnly(outputConfig))
  }

  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Ar(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.readCmd)).zipped.map(_ <> _)
  cmdArbiter.io.output <> io.output.readCmd

  io.output.readCmd.id.removeAssignments()
  io.output.readCmd.id := (cmdArbiter.io.chosen @@ cmdArbiter.io.output.id)

  // Route readResp
  val idPathRange = outputConfig.idWidth-1 downto outputConfig.idWidth - log2Up(inputsCount)
  val readRspIndex = io.output.readRsp.id(idPathRange)
  val readRspSels = (0 until inputsCount).map(readRspIndex === _)
  for((input,sel)<- (io.inputs,readRspSels).zipped){
    input.readRsp.valid := io.output.readRsp.valid && sel
    input.readRsp.payload <> io.output.readRsp.payload
    input.readRsp.id.removeAssignments()
    input.readRsp.id := io.output.readRsp.id(idPathRange.low-1 downto 0)
  }
  io.output.readRsp.ready := io.inputs(readRspIndex).readRsp.ready
//  io.output.readRsp.ready := (readRspSels,io.inputs.map(_.readRsp.ready)).zipped.map(_ & _).reduce(_ | _)   Not optimal because can't optimize master with ready fixed to high
}






object Axi4WriteOnlyArbiter{
  def getInputConfig(outputConfig : Axi4Config,inputsCount : Int) = outputConfig.copy(idWidth = outputConfig.idWidth - log2Up(inputsCount))
}

//routeBufferSize Specify how many write cmd could be schedule before any write data transaction is transmitted
case class Axi4WriteOnlyArbiter(outputConfig: Axi4Config,
                                inputsCount : Int,
                                routeBufferSize : Int,
                                routeBufferLatency : Int = 0,
                                routeBufferS2mPipe : Boolean = false) extends Component {
  assert(routeBufferSize >= 1)
  val inputConfig = Axi4ReadOnlyArbiter.getInputConfig(outputConfig,inputsCount)
  val io = new Bundle{
    val inputs = Vec(slave(Axi4WriteOnly(inputConfig)),inputsCount)
    val output = master(Axi4WriteOnly(outputConfig))
  }

  // Route writeCmd
  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Aw(inputConfig),inputsCount)
  (cmdArbiter.io.inputs,io.inputs.map(_.writeCmd)).zipped.map(_ <> _)
  val (cmdOutputFork,cmdRouteFork) = StreamFork2(cmdArbiter.io.output)
  io.output.writeCmd << cmdOutputFork
  io.output.writeCmd.id.removeAssignments()
  io.output.writeCmd.id := (cmdArbiter.io.chosen @@ cmdArbiter.io.output.id)
  
  // Route writeData
  var routeBuffer = cmdRouteFork.translateWith(cmdArbiter.io.chosen).queueLowLatency(routeBufferSize, latency = routeBufferLatency)
  if(routeBufferS2mPipe) routeBuffer = routeBuffer.s2mPipe()
  val routeDataInput = io.inputs(routeBuffer.payload).writeData
  io.output.writeData.valid := routeBuffer.valid && routeDataInput.valid
  io.output.writeData.payload  := routeDataInput.payload
  io.inputs.zipWithIndex.foreach{case(input,idx) => {
    input.writeData.ready := routeBuffer.valid && io.output.writeData.ready && routeBuffer.payload === idx
  }}
  routeBuffer.ready := io.output.writeData.fire && io.output.writeData.last

  // Route writeResp
  val idPathRange = outputConfig.idWidth-1 downto outputConfig.idWidth - log2Up(inputsCount)
  val writeRspIndex = io.output.writeRsp.id(idPathRange)
  val writeRspSels = (0 until inputsCount).map(writeRspIndex === _)
  for((input,sel)<- (io.inputs,writeRspSels).zipped){
    input.writeRsp.valid := io.output.writeRsp.valid && sel
    input.writeRsp.payload <> io.output.writeRsp.payload
    input.writeRsp.id.removeAssignments()
    input.writeRsp.id := io.output.writeRsp.id(idPathRange.low-1 downto 0)
  }
  io.output.writeRsp.ready := io.inputs(writeRspIndex).writeRsp.ready
}


object Axi4SharedArbiter{
  def getInputConfig(outputConfig : Axi4Config,
                     readInputsCount : Int,
                     writeInputsCount : Int,
                     sharedInputsCount : Int) = {
    val readIdUsage = log2Up(readInputsCount + sharedInputsCount)
    val writeIdUsage = log2Up(writeInputsCount + sharedInputsCount)
    val sharedIdUsage = Math.max(writeIdUsage,readIdUsage)
    (
      outputConfig.copy(idWidth = outputConfig.idWidth - readIdUsage),
      outputConfig.copy(idWidth = outputConfig.idWidth - writeIdUsage),
      outputConfig.copy(idWidth = outputConfig.idWidth - sharedIdUsage)
    )
  }
}

//routeBufferSize Specify how many write cmd could be schedule before any write data transaction is transmitted
case class Axi4SharedArbiter(outputConfig: Axi4Config,
                             readInputsCount : Int,
                             writeInputsCount : Int,
                             sharedInputsCount : Int,
                             routeBufferSize : Int,
                             routeBufferLatency : Int = 0,
                             routeBufferM2sPipe : Boolean = false,
                             routeBufferS2mPipe : Boolean = false) extends Component {
  assert(routeBufferSize >= 1)
  val inputsCount = readInputsCount + writeInputsCount + sharedInputsCount
  val (readInputConfig,writeInputConfig,sharedInputConfig) = Axi4SharedArbiter.getInputConfig(outputConfig,readInputsCount,writeInputsCount,sharedInputsCount)

  val io = new Bundle{
    val readInputs   = Vec(slave(Axi4ReadOnly(readInputConfig)) ,readInputsCount)
    val writeInputs  = Vec(slave(Axi4WriteOnly(writeInputConfig)),writeInputsCount)
    val sharedInputs = Vec(slave(Axi4Shared(sharedInputConfig))   ,sharedInputsCount)
    val output = master(Axi4Shared(outputConfig))
  }

  val readRange   = 0 to readInputsCount -1
  val writeRange  = readRange.high + 1 to readRange.high + writeInputsCount
  val sharedRange = writeRange.high + 1 to writeRange.high + sharedInputsCount


  // Route writeCmd
  val inputsCmd = io.readInputs.map(axi => {
    val newPayload = Axi4Arw(sharedInputConfig)
    newPayload.assignSomeByName(axi.readCmd.payload)
    newPayload.write := False
    newPayload.id.removeAssignments()
    newPayload.id := axi.readCmd.id.resized
    axi.readCmd.translateWith(newPayload)
  }) ++ io.writeInputs.map(axi => {
    val newPayload = Axi4Arw(sharedInputConfig)
    newPayload.assignSomeByName(axi.writeCmd.payload)
    newPayload.write := True
    newPayload.id.removeAssignments()
    newPayload.id := axi.writeCmd.id.resized
    axi.writeCmd.translateWith(newPayload)
  }) ++ io.sharedInputs.map( axi => {
    axi.sharedCmd.translateWith(axi.sharedCmd.payload)
  })

  val cmdArbiter = StreamArbiterFactory.roundRobin.build(Axi4Arw(sharedInputConfig),inputsCount)
  (inputsCmd,cmdArbiter.io.inputs).zipped.map(_ drive _)
  val (cmdOutputFork,cmdRouteFork) = StreamFork2(cmdArbiter.io.output)
  io.output.sharedCmd << cmdOutputFork
  io.output.sharedCmd.id.removeAssignments()
  io.output.sharedCmd.id := Mux(
    sel       = cmdOutputFork.write,
    whenTrue  = OHToUInt(cmdArbiter.io.chosenOH(sharedRange) ## cmdArbiter.io.chosenOH(writeRange)) @@ cmdOutputFork.id,
    whenFalse = OHToUInt(cmdArbiter.io.chosenOH(sharedRange) ## cmdArbiter.io.chosenOH(readRange) ) @@ cmdOutputFork.id
  ).resized

  // Route writeData
  val writeLogic = if (writeInputsCount + sharedInputsCount != 0) new Area {
    @dontName val writeDataInputs = (io.writeInputs.map(_.writeData) ++ io.sharedInputs.map(_.writeData))
    var routeBuffer = cmdRouteFork.throwWhen(!cmdRouteFork.write).translateWith(OHToUInt(cmdArbiter.io.chosenOH(sharedRange) ## cmdArbiter.io.chosenOH(writeRange))).queueLowLatency(routeBufferSize, latency = routeBufferLatency)
    if(routeBufferM2sPipe) routeBuffer = routeBuffer.m2sPipe()
    if(routeBufferS2mPipe) routeBuffer = routeBuffer.s2mPipe()
    val routeDataInput = writeDataInputs.apply(routeBuffer.payload)
    io.output.writeData.valid := routeBuffer.valid && routeDataInput.valid
    io.output.writeData.payload := routeDataInput.payload
    writeDataInputs.zipWithIndex.foreach { case (input, idx) => {
      input.ready := routeBuffer.valid && io.output.writeData.ready && routeBuffer.payload === idx
    }}
    routeBuffer.ready := io.output.writeData.fire && io.output.writeData.last

    // Route writeResp
    @dontName val writeRspInputs = (io.writeInputs.map(_.writeRsp) ++ io.sharedInputs.map(_.writeRsp))
    val writeIdPathRange = log2Up(writeInputsCount + sharedInputsCount) + widthOf(cmdOutputFork.id) - 1 downto widthOf(cmdOutputFork.id)
    val writeRspIndex = io.output.writeRsp.id(writeIdPathRange)
    val writeRspSels = (0 until writeInputsCount + sharedInputsCount).map(writeRspIndex === _)
    for ((input, sel) <- (writeRspInputs, writeRspSels).zipped) {
      input.valid := io.output.writeRsp.valid && sel
      input.payload <> io.output.writeRsp.payload
      input.id.removeAssignments()
      input.id := io.output.writeRsp.id.resized
    }
    io.output.writeRsp.ready := writeRspInputs.read(writeRspIndex).ready
  } else new Area {
    cmdRouteFork.ready := True
    io.output.w.valid := False
    io.output.w.payload.assignDontCare()
    io.output.b.ready := True
  }

  // Route readResp
  @dontName val readRspInputs = (io.readInputs.map(_.readRsp) ++ io.sharedInputs.map(_.readRsp))
  val readIdPathRange = log2Up(readInputsCount + sharedInputsCount) + widthOf(cmdOutputFork.id) - 1 downto widthOf(cmdOutputFork.id)
  val readRspIndex = io.output.readRsp.id(readIdPathRange)
  val readRspSels = (0 until readInputsCount + sharedInputsCount).map(readRspIndex === _)
  for((input,sel)<- (readRspInputs,readRspSels).zipped){
    input.valid := io.output.readRsp.valid && sel
    input.payload <> io.output.readRsp.payload
    input.id.removeAssignments()
    input.id := io.output.readRsp.id.resized
  }
  io.output.readRsp.ready := readRspInputs.read(readRspIndex).ready
}
