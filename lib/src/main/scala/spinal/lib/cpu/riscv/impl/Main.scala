package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.mm._
import spinal.lib.tool.{InterruptReceiverTag, QSysify}

object CoreMain{
  import extension._

  class TopLevel extends Component{
    val io_interrupt = in Bool

//    implicit val p = CoreParm(
//      pcWidth = 32,
//      addrWidth = 32,
//      startAddress = 0x200,
//      regFileReadyKind = sync,
//      branchPrediction = dynamic,
//      bypassExecute0 = true,
//      bypassExecute1 = true,
//      bypassWriteBack0 = true,
//      bypassWriteBack1 = true,
//      collapseBubble = true,
//      instructionBusKind = cmdStream_rspFlow,
//      dataBusKind = cmdStream_rspFlow,
//      fastFetchCmdPcCalculation = true,
//      dynamicBranchPredictorCacheSizeLog2 = 7
//    )
  implicit val p = CoreParm(
    pcWidth = 32,
    addrWidth = 32,
    startAddress = 0x200,
    regFileReadyKind = sync,
    branchPrediction = dynamic,
    bypassExecute0 = true,
    bypassExecute1 = true,
    bypassWriteBack0 = true,
    bypassWriteBack1 = true,
    collapseBubble = true,
    instructionBusKind = cmdStream_rspStream,
    dataBusKind = cmdStream_rspFlow,
    fastFetchCmdPcCalculation = true,
    dynamicBranchPredictorCacheSizeLog2 = 7
  )
    p.add(new MulExtension)
    p.add(new DivExtension)
    p.add(new BarrelShifterFullExtension)
    p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=io_interrupt,IrqUsage(isException=false),name="io_interrupt"))
    //      p.add(new BarrelShifterLightExtension)
    val io = new Bundle{
      val i = master(CoreInstructionBus())
      val d = master(CoreDataBus())
    }

    val core = new Core

    def StreamDelay[T <: Data](that : Stream[T]) = that
    io.i.cmd << StreamDelay(core.io.i.cmd)
    io.d.cmd << StreamDelay(core.io.d.cmd)
    core.io.i.rsp << StreamDelay(io.i.rsp)
    core.io.d.rsp << StreamDelay(io.d.rsp)

  }

  def main(args: Array[String]) {
    SpinalVhdl({

      new TopLevel().setDefinitionName("CoreWrapper")
    }
    ,_.setLibrary("riscv"))
  }
}




object QSysAvalonCore{
  import extension._

  class RiscvAvalon extends Component{
    lazy val p = CoreParm(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = dynamic,
      bypassExecute0 = true,
      bypassExecute1 = true,
      bypassWriteBack0 = true,
      bypassWriteBack1 = true,
      collapseBubble = true,
      instructionBusKind = cmdStream_rspFlow,
      dataBusKind = cmdStream_rspFlow,
      fastFetchCmdPcCalculation = true,
      dynamicBranchPredictorCacheSizeLog2 = 7
    )

    val io = new Bundle{
      val i = master(AvalonMMBus(CoreInstructionBus.getAvalonConfig(p)))
      val d = master(AvalonMMBus(CoreDataBus.getAvalonConfig(p)))
      val interrupt = in(Bool)
    }
    val interrupt = False
    p.add(new MulExtension)
    p.add(new DivExtension)
    p.add(new BarrelShifterFullExtension)
    p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=interrupt,IrqUsage(isException=false),name="io_interrupt"))
    //      p.add(new BarrelShifterLightExtension)
    val core = new Core()(p)
    io.i <> core.io.i.toAvalon()
    io.d <> core.io.d.toAvalon()
  }

  def main(args: Array[String]) {
    //val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("lib_riscvAvalon"))
    val report = SpinalVhdl(new RiscvAvalon(),_.setLibrary("qsys").onlyStdLogicVectorTopLevelIo)
    //val report = SpinalVhdl(new RiscvAvalon())

    report.topLevel.io.i addTag(ClockDomainTag(report.topLevel.clockDomain))
    report.topLevel.io.d addTag(ClockDomainTag(report.topLevel.clockDomain))
    report.topLevel.io.interrupt addTag(InterruptReceiverTag(report.topLevel.io.i,report.topLevel.clockDomain))
    QSysify(report.topLevel)
  }
}