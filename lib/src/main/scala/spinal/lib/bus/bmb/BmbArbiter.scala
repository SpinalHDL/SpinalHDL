package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._




case class BmbArbiter(inputsParameters: Seq[BmbParameter], outputParameter : BmbParameter, portCount : Int, pendingRspMax : Int) extends Component{
  val io = new Bundle{
    val inputs = Vec(inputsParameters.map(p => slave(Bmb(p))))
    val output = master(Bmb(outputParameter))
  }
  val logic = if(portCount == 1) new Area{
    io.output << io.inputs(0)
  } else new Area {
    val inputSourceWidth = inputsParameters.map(_.sourceWidth).max
    val sourceRouteRange = inputSourceWidth + log2Up(inputsParameters.size)-1 downto inputSourceWidth
    assert(sourceRouteRange.high < outputParameter.sourceWidth, "Not enough source bits")

    val arbiterFactory = StreamArbiterFactory.lowerFirst.fragmentLock
    val arbiter = arbiterFactory.build(Fragment(BmbCmd(outputParameter)), portCount)

    //Connect arbiters inputs
    for((s, m) <- (arbiter.io.inputs, io.inputs.map(_.cmd)).zipped){
      s.arbitrationFrom(m)
      s.last := m.last
      s.weakAssignFrom(m)
    }

    //Connect arbiters outputs
    io.output.cmd << arbiter.io.output
    io.output.cmd.source(sourceRouteRange) := arbiter.io.chosen

    //Connect responses
    val rspSel = io.output.rsp.source(sourceRouteRange)
    for((input, index) <- io.inputs.zipWithIndex){
      input.rsp.valid := io.output.rsp.valid && rspSel === index
      input.rsp.last := io.output.rsp.last
      input.rsp.weakAssignFrom(io.output.rsp)
    }
    io.output.rsp.ready := io.inputs(rspSel).rsp.ready
  }
}