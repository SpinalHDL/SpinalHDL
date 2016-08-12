package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

case class Axi4ReadDecoder(axiConfig: Axi4Config,decodings : Iterable[SizeMapping],pendingMax : Int = 7) extends Component{
  assert(axiConfig.isReadOnly)
  val io = new Bundle{
    val input = slave(Axi4(axiConfig))
    val outputs = Vec(master(Axi4(axiConfig)),decodings.size)
  }

  val pendingCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.readCmd.fire,
    decWhen = io.input.readRsp.fire && io.input.readRsp.last
  )

  val decodedSels,appliedSels = Bits(decodings.size bits)
  val lastCmdSel = RegNextWhen(appliedSels,io.input.readCmd.ready)  init(0)
  val allowCmd = pendingCounter =/= pendingMax && (pendingCounter === 0  || (lastCmdSel === decodedSels))
  decodedSels  := decodings.map(_.hit(io.input.readCmd.addr) && io.input.readCmd.valid).asBits
  appliedSels := allowCmd ? decodedSels | 0

  io.input.readCmd.ready := (appliedSels & io.outputs.map(_.readCmd.ready).asBits).orR
  for((output,sel) <- (io.outputs,appliedSels.asBools).zipped){
    output.readCmd.valid := sel
    output.readCmd.payload := io.input.readCmd.payload
    output.readRsp.ready := io.input.readRsp.ready
  }

  val readRspIndex = OHToUInt(lastCmdSel)
  io.input.readRsp.valid := (lastCmdSel & io.outputs.map(_.readRsp.valid).asBits).orR
  io.input.readRsp.payload := io.outputs(readRspIndex).readRsp.payload
}


case class Axi4WriteDecoder(axiConfig: Axi4Config,decodings : Iterable[SizeMapping],pendingMax : Int = 7) extends Component{
  assert(axiConfig.isWriteOnly)
  val io = new Bundle{
    val input = slave(Axi4(axiConfig))
    val outputs = Vec(master(Axi4(axiConfig)),decodings.size)
  }

  val pendingCmdCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeRsp.fire
  )

  val pendingDataCounter = CounterUpDown(
    stateCount = pendingMax+1,
    incWhen = io.input.writeCmd.fire,
    decWhen = io.input.writeData.fire && io.input.writeData.last
  )

  val decodedCmdSels,appliedCmdSels = Bits(decodings.size bits)
  val lastCmdSel  = RegNextWhen(appliedCmdSels,io.input.writeCmd.ready)  init(0)
  val allowCmd    = pendingCmdCounter =/= pendingMax && (pendingCmdCounter === 0  || (lastCmdSel === decodedCmdSels))
  val allowData   = pendingDataCounter =/= 0
  decodedCmdSels := decodings.map(_.hit(io.input.writeCmd.addr) && io.input.writeCmd.valid).asBits
  appliedCmdSels := allowCmd ? decodedCmdSels | 0

  //Wire writeCmd
  io.input.writeCmd.ready := (appliedCmdSels & io.outputs.map(_.writeCmd.ready).asBits).orR
  for((output,sel) <- (io.outputs,appliedCmdSels.asBools).zipped){
    output.writeCmd.valid := sel
    output.writeCmd.payload := io.input.writeCmd.payload
  }

  //Wire writeData
  io.input.writeData.ready := (lastCmdSel & io.outputs.map(_.writeData.ready).asBits).orR
  for((output,linkEnable) <- (io.outputs,lastCmdSel.asBools).zipped){
    output.writeData.valid   := io.input.writeData.valid && linkEnable
    output.writeData.payload := io.input.writeData.payload
  }

  //Wire writeRsp
  val writeRspIndex = OHToUInt(lastCmdSel)
  io.input.writeRsp.valid := (lastCmdSel & io.outputs.map(_.writeRsp.valid).asBits).orR
  io.input.writeRsp.payload := io.outputs(writeRspIndex).writeRsp.payload
  io.outputs.foreach(_.writeRsp.ready := io.input.writeRsp.ready)
}
