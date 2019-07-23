package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._




case class BmbArbiter(p : BmbParameter,
                      portCount : Int,
                      pendingRspMax : Int,
                      lowerFirstPriority : Boolean) extends Component{
  val sourceRouteWidth = log2Up(portCount)
  val inputSourceWidth = p.sourceWidth - sourceRouteWidth
  val inputsParameter = p.copy(sourceWidth = inputSourceWidth)
  val sourceRouteRange = inputSourceWidth + sourceRouteWidth - 1 downto inputSourceWidth
  val sourceInputRange = inputSourceWidth - 1 downto 0
  assert(inputSourceWidth >= 0)

  val io = new Bundle{
    val inputs = Vec.fill(portCount)(slave(Bmb(inputsParameter)))
    val output = master(Bmb(p))
  }

  val logic = if(portCount == 1) new Area{
    io.output << io.inputs(0)
  } else new Area {
    assert(sourceRouteRange.high < p.sourceWidth, "Not enough source bits")

    val arbiterFactory = StreamArbiterFactory.fragmentLock
    if(lowerFirstPriority) {
      arbiterFactory.lowerFirst
    } else {
      arbiterFactory.roundRobin
    }
    val arbiter = arbiterFactory.build(Fragment(BmbCmd(p)), portCount)

    //Connect arbiters inputs
    for((s, m) <- (arbiter.io.inputs, io.inputs.map(_.cmd)).zipped){
      s.arbitrationFrom(m)
      s.last := m.last
      s.weakAssignFrom(m)
    }

    //Connect arbiters outputs
    io.output.cmd << arbiter.io.output
    io.output.cmd.source.removeAssignments()
    io.output.cmd.source(sourceRouteRange) := arbiter.io.chosen
    io.output.cmd.source(sourceInputRange) := arbiter.io.output.source(sourceInputRange)

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

object BmbArbiter{
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new BmbArbiter(
      p = BmbParameter(
        addressWidth = 16,
        dataWidth = 32,
        lengthWidth = 5,
        sourceWidth = 2,
        contextWidth = 3
      ),
      portCount = 4,
      pendingRspMax = 3,
      lowerFirstPriority = false
    ))
  }
}