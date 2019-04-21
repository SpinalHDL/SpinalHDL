package spinal.lib.bus.bmb

import spinal.core._
import spinal.lib._



//
//case class BmbArbiter(p: BmbParameter, portCount : Int, pendingRspMax : Int) extends Component{
//  val io = new Bundle{
//    val inputs = Vec(slave(Bmb(p)), portCount)
//    val output = master(Bmb(p))
//  }
//  val logic = if(portCount == 1) new Area{
//    io.output << io.inputs(0)
//  } else new Area {
//    val arbiterFactory = StreamArbiterFactory.lowerFirst.transactionLock
//    val arbiter = arbiterFactory.build(PipelinedMemoryBusCmd(pipelinedMemoryBusConfig), portCount)
//    (arbiter.io.inputs, io.inputs).zipped.foreach(_ <> _.cmd)
//
//    val rspRouteOh = Bits(portCount bits)
//
//    val rsp = if(!rspRouteQueue) new Area{
//      assert(pendingRspMax == 1)
//      val pending = RegInit(False) clearWhen(io.output.rsp.valid)
//      val target = Reg(Bits(portCount bits))
//      rspRouteOh := target
//      when(io.output.cmd.fire && !io.output.cmd.write){
//        target  := arbiter.io.chosenOH
//        pending := True
//      }
//      io.output.cmd << arbiter.io.output.haltWhen(pending && !io.output.rsp.valid)
//    } else new Area{
//      val (outputCmdFork, routeCmdFork) = StreamFork2(arbiter.io.output)
//      io.output.cmd << outputCmdFork
//
//      val rspRoute = routeCmdFork.translateWith(arbiter.io.chosenOH).throwWhen(routeCmdFork.write).queueLowLatency(size = pendingRspMax, latency = 1)
//      rspRoute.ready := io.output.rsp.valid
//      rspRouteOh := rspRoute.payload
//    }
//
//    for ((input, id) <- io.inputs.zipWithIndex) {
//      input.rsp.valid := io.output.rsp.valid && rspRouteOh(id)
//      input.rsp.payload := io.output.rsp.payload
//    }
//  }
//}