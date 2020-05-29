package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

object BmbSourceDecoder {
  def outputParameter(inputParameter : BmbParameter) = inputParameter.copy(sourceWidth = 0)
}

case class BmbSourceDecoder(inputParameter : BmbParameter) extends Component{
  val outputParameter = BmbSourceDecoder.outputParameter(inputParameter)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val outputs = Vec(master(Bmb(outputParameter)), 1 << inputParameter.sourceWidth)
  }

//  io.input.cmd.ready := False
//  for((output, outputId) <- io.outputs.zipWithIndex){
//    val hit = io.input.cmd.source === outputId
//    output.cmd.valid := io.input.cmd.valid && hit
//    output.cmd.payload := io.input.cmd.payload
//    io.input.cmd.ready setWhen(output.cmd.ready && hit)
//  }
//
  Vec(io.outputs.map(_.cmd)) <> StreamDemux(io.input.cmd, io.input.cmd.source, io.outputs.size)
  val arbiter = StreamArbiterFactory.fragmentLock.roundRobin.build(Fragment(BmbRsp(inputParameter)), 1 << inputParameter.sourceWidth)
  arbiter.io.inputs <> Vec(io.outputs.map(_.rsp))
  arbiter.io.output <> io.input.rsp

  for(id <- 0 until 1 << inputParameter.sourceWidth){
    arbiter.io.inputs(id).source.removeAssignments() := id
    io.outputs(id).cmd.source.removeAssignments()
  }
}
