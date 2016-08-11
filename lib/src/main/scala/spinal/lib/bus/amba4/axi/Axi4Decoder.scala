package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc.SizeMapping

case class Axi4ReadDecoder(axiConfig: Axi4Config,decodings : Iterable[SizeMapping],pendingMax : Int = 3) extends Component{
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
  val lastId     = RegNextWhen(io.input.readCmd.id,io.input.readCmd.ready)
  val allowCmd = pendingCounter === 0 || (lastCmdSel === decodedSels && lastId === io.input.readCmd.id)
  decodedSels  := decodings.map(_.hit(io.input.readCmd.addr) && io.input.readCmd.valid).asBits
  appliedSels := allowCmd ? decodedSels | 0

  io.input.readCmd.ready := (appliedSels & io.outputs.map(_.readCmd.ready).asBits).orR
  for((output,sel) <- (io.outputs,appliedSels.asBools).zipped){
    output.readCmd.valid := sel
    output.readCmd.payload := io.input.readCmd.payload
    output.readRsp.ready := io.input.readRsp.ready
  }

  val readRspIndex = OHToUInt(lastId)
  io.input.readRsp.valid := (lastCmdSel & io.outputs.map(_.readRsp.valid).asBits).orR
  io.input.readRsp.payload := io.outputs(readRspIndex).readRsp.payload
}
