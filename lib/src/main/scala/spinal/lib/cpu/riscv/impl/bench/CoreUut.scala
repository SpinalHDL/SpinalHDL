package spinal.lib.cpu.riscv.impl.bench

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.impl._

object CoreUut{
  import spinal.lib.cpu.riscv.impl.extension._

  class TopLevel(p : RiscvCoreConfig,iCacheConfig : InstructionCacheConfig, dCacheConfig : DataCacheConfig,debug : Boolean,interruptCount : Int) extends Component{
    implicit val toto = p
    val oneCycleInstrPip = true
    val iCached = iCacheConfig != null
    val dCached = dCacheConfig != null

    val io = new Bundle{
      val i_cmd = master Stream CoreInstructionCmd()
      val i_rsp = slave Stream (new Bundle{
        val instruction = Bits(32 bit)
        val pc = UInt(p.addrWidth bit)
      })
      val d = master(CoreDataBus())
      val iCheck = master(Flow((new Bundle{
        val address = UInt(p.addrWidth bit)
        val data = Bits(32 bit)
      })))
      val iCmdDrive = in Bool
      val iRspDrive = in Bool
      val dCmdDrive = in Bool
      val dRspDrive = in Bool
      val doCacheFlush = in Bool

      //debug purposes
      val cpuCmdLog = master Flow(CoreDataCmd())
      val cpuRspLog = master Flow(Bits(32 bits))
      val interrupt = in Bits(interruptCount bits)
    }

    if(interruptCount != 0)
      p.add(new SimpleInterruptExtension(exceptionVector=0x0).addIrq(id=4,pins=io.interrupt,IrqUsage(isException=false),name="io_interrupt"))
    val nativeInstructionBusExtension = if(!iCached)p.add(new NativeInstructionBusExtension)  else null
    val cachedInstructionBusExtension = if(iCached)p.add(new CachedInstructionBusExtension(iCacheConfig,false,false))  else null
    val nativeDataBusExtension = if(!dCached) p.add(new NativeDataBusExtension) else null
    val cachedDataBusExtension = if(dCached) p.add(new CachedDataBusExtension(dCacheConfig,true)) else null



    def StreamDelay[T <: Data](that : Stream[T]) = that.s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe()
    def InstStreamDelay[T <: Data](that : Stream[T]) = if(oneCycleInstrPip) that else that.s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe().s2mPipe()
    val core = new RiscvCore



    val iLogic = if(iCached) new Area{
      val i_cmd = cloneOf(io.i_cmd)
      val i_rsp = cloneOf(io.i_rsp)
      io.i_cmd << StreamDelay(i_cmd.continueWhen(io.iCmdDrive))
      i_rsp << StreamDelay(io.i_rsp).continueWhen(io.iRspDrive)

      val memBus = cachedInstructionBusExtension.memBus

      i_rsp.ready := True


      val busy = RegInit(False)
      val burstCounter = Reg(UInt(log2Up(iCacheConfig.bytePerLine/4) bit))
      val wrapCounter = Reg(UInt(log2Up(iCacheConfig.bytePerLine/4) bit))
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

      //Emit ramdom instruction cache flush
    /* Component.push(core)
      val flushEmitter = EventEmitter(core.iCacheFlush.cmd)
      when(io.doCacheFlush.pull){
        flushEmitter.emit()
      }
      Component.pop(core)*/
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
    val dLogic = if(dCached) new Area{
      val d_cmd = cloneOf(io.d.cmd)
      val d_rsp = cloneOf(io.d.rsp)
      io.d.cmd << StreamDelay(d_cmd.continueWhen(io.dCmdDrive))
      d_rsp << StreamDelay(io.d.rsp).continueWhen(io.dRspDrive)

      val memBus = cachedDataBusExtension.memBus

      d_rsp.ready := True


      val busy = RegInit(False)
      val burstCounter = Reg(UInt(log2Up(dCacheConfig.bytePerLine/4 + 1) bit))
      val burstWriteLeft = Reg(UInt(log2Up(dCacheConfig.bytePerLine/4 + 1) bit)) init(0)
      val burstLength = Reg(UInt(log2Up(dCacheConfig.bytePerLine/4 + 1) bit)) init(0)
      val address = Reg(memBus.cmd.address)

      memBus.cmd.ready := False
      d_cmd.valid := False
      d_cmd.payload.assignDontCare()


      when(!busy){
        when(memBus.cmd.valid && io.iCmdDrive){
          when(!memBus.cmd.wr) {
            memBus.cmd.ready := True
            busy := True
            address := memBus.cmd.address
            burstCounter := 0
            burstLength := memBus.cmd.length
          } otherwise {
            d_cmd.valid := True
            d_cmd.size := CountOne(memBus.cmd.mask.asBits).mux(
              1 -> U(0),
              2 -> U(1),
              4 -> U(2),
              default -> U(0)
            )
            d_cmd.data := memBus.cmd.data
            d_cmd.wr := True
            when(d_cmd.ready) {
              memBus.cmd.ready := True
              when(burstWriteLeft === 0) {
                burstWriteLeft := memBus.cmd.length - 1
                d_cmd.address := memBus.cmd.address + LeastSignificantBitSet(memBus.cmd.mask)
                burstCounter := 1
              } otherwise {
                burstWriteLeft := burstWriteLeft - 1
                d_cmd.address := memBus.cmd.address + (burstCounter << 2)
                burstCounter := burstCounter + 1
              }
            }
          }
        }
      }otherwise{
        d_cmd.valid := True
        d_cmd.address := address + (burstCounter << 2)
        d_cmd.size := 2
        d_cmd.wr := False
        when(d_cmd.ready){
          burstCounter := burstCounter + 1
          when(burstCounter === burstLength-1){
            busy := False
          }
        }
      }
      memBus.rsp.valid <> d_rsp.valid
      memBus.rsp.data <> d_rsp.payload
    }else new Area{
      val memCpu = nativeDataBusExtension.memBus
      io.d.cmd << StreamDelay(memCpu.cmd.continueWhen(io.dCmdDrive))
      memCpu.rsp << StreamDelay(io.d.rsp.m2sPipe()).continueWhen(io.dRspDrive)
    }


    io.iCheck.valid := core.execute0.outInst.valid.pull
    io.iCheck.address := core.execute0.outInst.pc.pull
    io.iCheck.data := core.execute0.outInst.instruction.pull

    io.cpuCmdLog.valid := core.dCmd.valid.pull && core.dCmd.ready.pull
    io.cpuCmdLog.payload := core.dCmd.payload.pull

    io.cpuRspLog.valid := core.dRsp.valid.pull
    io.cpuRspLog.payload := core.writeBack.dataRspFormated.pull
  }

  def main(args: Array[String]) {
    def factory = {
        val iCacheConfig = InstructionCacheConfig(
          cacheSize = 4096*2,
          bytePerLine =32,
          wayCount = 1,
          wrappedMemAccess = true,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32
        )

        val dCacheConfig = DataCacheConfig(
          cacheSize = 4096*2,
          bytePerLine =32,
          wayCount = 1,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32
        )

        implicit val p = RiscvCoreConfig(
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
          fastFetchCmdPcCalculation = true,
          dynamicBranchPredictorCacheSizeLog2 = 16,
          branchPredictorHistoryWidth = 2
        )

        p.add(new MulExtension)
        p.add(new DivExtension)
        p.add(new BarrelShifterFullExtension)
//     p.add(new BarrelShifterLightExtension)



//      val iCacheConfig = null
//      val dCacheConfig = null
//
//      implicit val p = CoreConfig(
//        pcWidth = 32,
//        addrWidth = 32,
//        startAddress = 0x200,
//        regFileReadyKind = sync,
//        branchPrediction = disable,
//        bypassExecute0 = false,
//        bypassExecute1 = false,
//        bypassWriteBack = false,
//        bypassWriteBackBuffer = false,
//        collapseBubble = false,
//        fastFetchCmdPcCalculation = false,
//        dynamicBranchPredictorCacheSizeLog2 = 16,
//        branchPredictorHistoryWidth = 2
//      )
//
//      p.add(new MulExtension)
//      p.add(new DivExtension)
//      //p.add(new BarrelShifterFullExtension)
//      p.add(new BarrelShifterLightExtension)
      new TopLevel(p,iCacheConfig,dCacheConfig,true,4).setDefinitionName("CoreWrapper")
    }
    SpinalVhdl(factory)
    SpinalVerilog(factory)
  }
}

