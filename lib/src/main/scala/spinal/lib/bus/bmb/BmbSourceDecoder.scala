package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._

object BmbSourceDecoder {
  def outputParameters(inputParameter : BmbAccessParameter) = inputParameter.sources.values.map(s => inputParameter.withSingleSource(s))
}

case class BmbSourceDecoder(inputParameter : BmbParameter) extends Component{
  val outputParameters = BmbSourceDecoder.outputParameters(inputParameter.access)
  val io = new Bundle {
    val input = slave(Bmb(inputParameter))
    val outputs = Vec(outputParameters.map(p => master(Bmb(p))))
  }

  Vec(io.outputs.map(_.cmd)) <> StreamDemux(io.input.cmd, io.input.cmd.source, io.outputs.size)
  val arbiter = StreamArbiterFactory.fragmentLock.roundRobin.build(Fragment(BmbRsp(inputParameter)), outputParameters.size)
  arbiter.io.inputs <> Vec(io.outputs.map(_.rsp))
  arbiter.io.output <> io.input.rsp

  for(id <- 0 until 1 << outputParameters.size){
    arbiter.io.inputs(id).source.removeAssignments() := id
    io.outputs(id).cmd.source.removeAssignments()
  }
}
