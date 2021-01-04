package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._




case class BmbArbiter(inputsParameter : Seq[BmbParameter],
                      outputParameter : BmbParameter,
                      lowerFirstPriority : Boolean,
                      pendingInvMax : Int = 0) extends Component{
  val portCount = inputsParameter.size
  val sourceRouteWidth = log2Up(portCount)
  val sourceRouteRange = sourceRouteWidth-1 downto 0
//  val sourceInputRange = input.access.sourceWidth - 1 downto sourceRouteWidth


  val io = new Bundle{
    val inputs = Vec(inputsParameter.map(p => slave(Bmb(p))))
    val output = master(Bmb(outputParameter))
  }

  val bypass = (portCount == 1) generate new Area{
    io.output << io.inputs(0)
  }

  val memory = (portCount > 1) generate new Area {
//    assert(sourceRouteRange.high < p.access.sourceWidth, "Not enough source bits")

    val arbiterFactory = StreamArbiterFactory.fragmentLock
    if(lowerFirstPriority) arbiterFactory.lowerFirst else arbiterFactory.roundRobin

    val arbiter = arbiterFactory.build(Fragment(BmbCmd(outputParameter)), portCount)

    //Connect arbiters inputs
    for((s, m) <- (arbiter.io.inputs, io.inputs.map(_.cmd)).zipped){
      s.arbitrationFrom(m)
      s.last := m.last
      s.weakAssignFrom(m)
    }

    //Connect arbiters outputs
    io.output.cmd << arbiter.io.output
    io.output.cmd.source.removeAssignments()
    io.output.cmd.source := (arbiter.io.output.source @@ arbiter.io.chosen).resized

    //Connect responses
    val rspSel = io.output.rsp.source(sourceRouteRange)
    for((input, index) <- io.inputs.zipWithIndex){
      input.rsp.valid := io.output.rsp.valid && rspSel === index
      input.rsp.last := io.output.rsp.last
      input.rsp.weakAssignFrom(io.output.rsp.payload)
      input.rsp.source.removeAssignments() := (io.output.rsp.source >> sourceRouteWidth).resized
    }
    io.output.rsp.ready := io.inputs.map(_.rsp.ready).read(rspSel)
  }

  val invalidate = (portCount > 1 && outputParameter.invalidation.canInvalidate) generate new Area {
    assert(pendingInvMax != 0)
    val (inputs, inputsIndex) = (for(inputId <- 0 until portCount if inputsParameter(inputId).invalidation.canInvalidate) yield (io.inputs(inputId), inputId)).unzip

    val invCounter = CounterUpDown(
      stateCount = 2 << log2Up(pendingInvMax),
      incWhen    = io.output.inv.fire,
      decWhen    = io.output.ack.fire
    )

    val haltInv = invCounter.msb
    val forks = StreamFork(io.output.inv.haltWhen(haltInv), inputs.size)
    val logics = for(((input, inputId), forkId) <- (inputs, inputsIndex).zipped.toSeq.zipWithIndex) yield new Area{
      val ackCounter = CounterUpDown(
        stateCount = 2 << log2Up(pendingInvMax),
        incWhen    = input.ack.fire,
        decWhen    = io.output.ack.fire
      )
      input.inv.arbitrationFrom(forks(forkId))
      input.inv.address := forks(forkId).address
      input.inv.length := forks(forkId).length
      input.inv.source := (forks(forkId).source >> sourceRouteWidth).resized
      input.inv.all := io.output.inv.all || io.output.inv.source(sourceRouteRange) =/= inputId

      input.ack.ready := True
    }

    io.output.ack.valid := logics.map(_.ackCounter =/= 0).toSeq.andR
  }

  val sync = (portCount > 1 && outputParameter.invalidation.canSync) generate new Area{
    val syncSel = io.output.sync.source(sourceRouteRange)
    for((input, index) <- io.inputs.zipWithIndex){
      input.sync.valid := io.output.sync.valid && syncSel === index
      input.sync.source := io.output.sync.source.resized
    }
    io.output.sync.ready := io.inputs.map(_.sync.ready).read(syncSel)
  }
}

//object BmbArbiter{
//  def main(args: Array[String]): Unit = {
//    SpinalVerilog(new BmbArbiter(
//      p = BmbParameter(
//        addressWidth = 16,
//        dataWidth = 32,
//        lengthWidth = 5,
//        sourceWidth = 2,
//        contextWidth = 3
//      ),
//      portCount = 4,
//      lowerFirstPriority = false
//    ))
//  }
//}