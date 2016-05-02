package spinal.lib.cpu.riscv.impl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon._
import spinal.lib.tool.{ResetEmitterTag, InterruptReceiverTag, QSysify}

object CoreMain{
  import extension._

  class TopLevel extends Component{
    val io_interrupt = in Bool
    val oneCycleInstrPip = true
    val cached = true
    val cacheParam = InstructionCacheConfig(  cacheSize = 4096,
      bytePerLine =32,
      wayCount = 1,
      wrappedMemAccess = true,
      addressWidth = 32,
      cpuDataWidth = 32,
      memDataWidth = 32)

    implicit val p = CoreConfig(
      pcWidth = 32,
      addrWidth = 32,
      startAddress = 0x200,
      regFileReadyKind = sync,
      branchPrediction = dynamic,
      bypassExecute0 = true,
      bypassExecute1 = true,
      bypassWriteBack = true,
      bypassWriteBackBuffer = true,
      collapseBubble = true,
      dataBusKind = cmdStream_rspFlow,
      fastFetchCmdPcCalculation = true,
      dynamicBranchPredictorCacheSizeLog2 = 16,
      branchPredictorHistoryWidth = 2
    )

    p.add(new MulExtension)
    p.add(new DivExtension)
    p.add(new BarrelShifterFullExtension)
    p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pin=io_interrupt,IrqUsage(isException=false),name="io_interrupt"))
    // p.add(new BarrelShifterLightExtension)
    val nativeInstructionBusExtension = if(!cached)p.add(new NativeInstructionBusExtension)  else null
    val cachedInstructionBusExtension = if(cached)p.add(new CachedInstructionBusExtension(cacheParam))  else null


    val io = new Bundle{
      val i_cmd = master Stream CoreInstructionCmd()
      val i_rsp = slave Stream wrap(new Bundle{
        val instruction = Bits(32 bit)
        val pc = UInt(p.addrWidth bit)
      })
      val d = master(CoreDataBus())
      val iCheck = master(Flow(wrap(new Bundle{
        val address = UInt(p.addrWidth bit)
        val data = Bits(32 bit)
      })))
      val iCmdDrive = in Bool
      val iRspDrive = in Bool
      val dCmdDrive = in Bool
      val dRspDrive = in Bool
      val doCacheFlush = in Bool
    }
    def StreamDelay[T <: Data](that : Stream[T]) = that.s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe()
    def InstStreamDelay[T <: Data](that : Stream[T]) = if(oneCycleInstrPip) that else that.s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe()
    val core = new Core



    val iLogic = if(cached) new Area{
      val i_cmd = io.i_cmd.clone
      val i_rsp = io.i_rsp.clone
      io.i_cmd << StreamDelay(i_cmd.continueWhen(io.iCmdDrive))
      i_rsp << StreamDelay(io.i_rsp).continueWhen(io.iRspDrive)

      val memBus = cachedInstructionBusExtension.memBus

      i_rsp.ready := True


      val busy = RegInit(False)
      val burstCounter = Reg(UInt(log2Up(cacheParam.bytePerLine/4) bit))
      val wrapCounter = Reg(UInt(log2Up(cacheParam.bytePerLine/4) bit))
      val address = Reg(memBus.cmd.address)

      memBus.cmd.ready := False
      i_cmd.valid := False
      i_cmd.pc := address + (wrapCounter << 2)
      when(!busy){
        when(memBus.cmd.valid && io.iCmdDrive){
          busy := True
          memBus.cmd.ready := True
          address := memBus.cmd.address(31 downto 5) @@ U"00000"
          burstCounter := 0
          wrapCounter := memBus.cmd.address(4 downto 2)
        }
      }otherwise{
        i_cmd.valid := True
        when(i_cmd.ready){
          burstCounter := burstCounter + 1
          wrapCounter := wrapCounter + 1
          when(burstCounter === burstCounter.maxValue){
            busy := False
          }
        }
      }
      memBus.rsp.valid <> i_rsp.valid
      memBus.rsp.data <> i_rsp.instruction

      Component.push(core)
      val flushEmitter = EventEmitter(core.prefetch.iCacheFlush.cmd)
      when(io.doCacheFlush.pull){
        flushEmitter.emit()
      }
      Component.pop(core)
    }else {
      val coreIBus = nativeInstructionBusExtension.memBus
      io.i_cmd << InstStreamDelay(coreIBus.cmd.continueWhen(io.iCmdDrive))
      val rsp = Stream(CoreInstructionRsp())
      rsp.valid := io.i_rsp.valid
      rsp.pc := io.i_rsp.pc
      rsp.instruction := io.i_rsp.instruction
      coreIBus.rsp << InstStreamDelay(rsp).continueWhen(io.iRspDrive)
      io.i_rsp.ready := rsp.ready

      if(p.branchPrediction == dynamic){
        coreIBus.branchCachePort.cmd.valid := coreIBus.cmd.fire
        coreIBus.branchCachePort.cmd.payload := coreIBus.cmd.pc(2,p.dynamicBranchPredictorCacheSizeLog2 bit)
        rsp.branchCacheLine := coreIBus.branchCachePort.rsp
      }
    }
    io.d.cmd << StreamDelay(core.io.d.cmd.continueWhen(io.dCmdDrive))
    core.io.d.rsp << StreamDelay(io.d.rsp.m2sPipe()).continueWhen(io.dRspDrive)

    io.iCheck.valid := core.execute0.outInst.valid.pull
    io.iCheck.address := core.execute0.outInst.pc.pull
    io.iCheck.data := core.execute0.outInst.instruction.pull
  }

  def main(args: Array[String]) {
    SpinalVhdl({ new TopLevel().setDefinitionName("CoreWrapper")}
    ,_.setLibrary("riscv"))
    SpinalVhdl({ new TopLevel()})
  }
}