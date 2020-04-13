package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._




case class BmbArbiter(p : BmbParameter,
                      portCount : Int,
                      pendingRspMax : Int,
                      lowerFirstPriority : Boolean,
                      inputsWithInv : Seq[Boolean] = null,
                      inputsWithSync : Seq[Boolean] = null,
                      pendingInvMax : Int = 0) extends Component{
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

  val bypass = (portCount == 1) generate new Area{
    io.output << io.inputs(0)
    if(p.canInvalidate){
      io.inputs(0).inv << io.output.inv
      io.inputs(0).ack >> io.output.ack
    }
  }

  val memory = (portCount > 1) generate new Area {
    assert(sourceRouteRange.high < p.sourceWidth, "Not enough source bits")

    val arbiterFactory = StreamArbiterFactory.fragmentLock
    if(lowerFirstPriority) arbiterFactory.lowerFirst else arbiterFactory.roundRobin

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

  val invalidate = (portCount > 1 && p.canInvalidate) generate new Area {
    assert(inputsWithInv != null)
    assert(pendingInvMax != 0)
    val (inputs, inputsIndex) = (for(inputId <- 0 until portCount if inputsWithInv(inputId)) yield (io.inputs(inputId), inputId)).unzip

    val invCounter = CounterUpDown(
      stateCount = log2Up(pendingInvMax) + 1,
      incWhen    = io.output.inv.fire,
      decWhen    = io.output.ack.fire
    )

    val haltInv = invCounter.msb
    val forks = StreamFork(io.output.inv.haltWhen(haltInv), inputs.size)
    val logics = for((input, inputId) <- (inputs, inputsIndex).zipped) yield new Area{
      val ackCounter = CounterUpDown(
        stateCount = log2Up(pendingInvMax) + 1,
        incWhen    = input.ack.fire,
        decWhen    = io.output.ack.fire
      )
      input.inv.arbitrationFrom(forks(inputId))
      input.inv.address := forks(inputId).address
      input.inv.length := forks(inputId).length
      input.inv.source := forks(inputId).source(sourceInputRange)
      input.inv.all := io.output.inv.all || io.output.inv.source(sourceRouteRange) =/= inputId
    }

    io.output.ack.valid := logics.map(_.ackCounter =/= 0).toSeq.andR
  }

  val sync = p.canSync generate new Area{
    assert(inputsWithSync != null)

    val syncSel = io.output.sync.source(sourceRouteRange)
    for((input, index) <- io.inputs.zipWithIndex){
      input.sync.valid := io.output.sync.valid && syncSel === index
      input.sync.source := io.output.sync.source
    }
    io.output.sync.ready := io.inputs(syncSel).sync.ready
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